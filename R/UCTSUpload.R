#' @include common.R
#' @include classConstructor.R
#' @include wrapper.R
#'
#' @name dotEncryptPassword
#' @title Encrypt the Datastream password
#' @description This is a port of the VBA code
#'
#' @param strPassword the password to be encrypted
#' @return an encrypted password
#'
#'
#' @keywords internal
#'
.EncryptPassword <- function(strPassword=""){


  iSeed <- as.raw(199L) # arbitrary number
  strCrypted <- ""
  bBytes <- charToRaw(strPassword)

  for(b in bBytes)
  {
    iCryptedByte <- as.raw(xor(b , iSeed))
    strCrypted <- paste0(strCrypted, formatC(as.integer(iCryptedByte),digits=3,width=3,flag="0"))
    # add previous byte, XOR with arbitrary value
    iSeed <- xor(as.raw((as.integer(iSeed) + as.integer(iCryptedByte)) %% 255L), as.raw(67L))
  }

  return(strCrypted)
}



#' @name dotgetTimeseries
#' @title convert xts timeseries into a string that can be sent to
#' the Datastream server
#'
#' @param Data the xts timeseries to be converted
#' @param freq the frequency of the data
#' @param digits the number of decimal places to round the data to
#' @param NA_VALUE the string to replace NA data with
#'
#' @return A string of the core data of Data
#'
#'
#' @importFrom zoo zoo index
#' @importFrom xts merge.xts .indexwday
#' @importFrom stringr str_trim
#' @keywords internal
#'
.getTimeseries <- function(Data, freq, digits, NA_VALUE){
  if(ncol(Data) > 1) {
    # Make sure we are only dealing with a single column xts
    Data <- Data[,1]
  }
  if (freq == "D")
  {

    # We have a daily frequency, which means we need to do more work matching up the dates as
    # Datastream assumes that they are in weekday order.  The loaded timeseries might have gaps or weekend
    # measures
    # the xts .indexwday gives the day of the week with 0=Sunday and 6=Saturday

    # We need to make sure there are no blanks in the data
    startDate <- zoo::index(first(Data))
    endDate <- zoo::index(last(Data))
    NADates <- seq(from=startDate, to=endDate, by="days")
    NAData <- zoo(c(NA), order.by=NADates)
    #merge and fill missing rows with NAs
    wData <- xts::merge.xts(Data, NAData, fill=NA)

    # This only picks the weeksdays from the original series
    wData <- wData[which(xts::.indexwday(wData) %in% 1:5),1]
  }else{
    wData <- Data
    #If we do not have a daily frequency then we can just load up the datapoints, with the implicit
    #assumption that they are in the right frequency
  }

  sFormattedData <- suppressWarnings(formatC(wData, digits = digits, mode="double", format="f"))
  sFormattedData <- stringr::str_trim(sFormattedData)

  #We need to make sure that any missing data is replaced with the
  # the correct symbol
  sFormattedData[which(sFormattedData=="NaN")] <- NA_VALUE

  #Collapse the array into a string
  sData<-paste0(sFormattedData,collapse=",")
  sData<-paste0(sData,",")

  return(sData)
}



#' @title Upload a UCTS timeseries into Datastream
#'
#' @description Uploads an xts into a UCTS in the Datastream Database
#' @details Note this function does not check to see if there is
#'  a pre-existing timeseries already in Datastream.  It will just overwrite
#'   any existing UCTS.
#' @param tsData - an xts (or timeseries object that can be converted to
#' one) to be uploaded.
#' @param TSCode  The mnemonic of the target UCTS
#' @param MGMTGroup Must have managment group.  Only the first
#' characters will be used.
#' @param freq The frequency of the data to be uploaded
#' @param seriesName the name of the series
#' @param Units Units of the data - can be no more than 12 characters -
#'  excess will be trimmed to that length
#' @param Decimals Number of Decimals in the data - a number between 0 and
#'  9 - if outside that range then trimmed
#' @param ActPer Whether the values are percentages ("N") or actual
#' numbers ("Y")
#' @param freqConversion How to do any FX conversions
#' @param Alignment Alignment of the data within periods
#' @param Carry whether to carry data over missing dates
#' @param PrimeCurr the currency of the timeseries
#' @param strUsername your Datastream username
#' @param strPassword your Datastream Password
#' @param strServerName URL of the Datastream server
#' @param strServerPage page on the datastream server
#' @return TRUE if the upload has been a success, otherwise an error message
#'
#' @export
#'
#' @importFrom zoo index
#' @importFrom httr POST add_headers content content_type
#' @importFrom xts as.xts first last xtsible
#'
UCTSUpload <- function(tsData,
                       TSCode="",
                       MGMTGroup="ABC",
                       freq = c("D","W","M","Q","Y"),
                       seriesName,
                       Units="",
                       Decimals=2,
                       ActPer=c("N","Y"),
                       freqConversion= c("ACT","SUM","AVG","END"),
                       Alignment=c("1ST","MID","END"),
                       Carry=c("YES","NO","PAD"),
                       PrimeCurr="",
                       strUsername = ifelse(Sys.getenv("DatastreamUsername") != "",
                                            Sys.getenv("DatastreamUsername"),
                                            options()$Datastream.Username),
                       strPassword = ifelse(Sys.getenv("DatastreamPassword") != "",
                                            Sys.getenv("DatastreamPassword"),
                                            options()$Datastream.Password),
                       strServerName="https://product.datastream.com",
                       strServerPage="/UCTS/UCTSMaint.asp"){

  #Check inputs are valid

  if(!xtsible(tsData)){
    stop(paste0("tsData must be a time-based object and not of class ",class(tsData)))

  }

  if(!freq[1] %in% c("D","W","M","Q","Y")){
    stop("freq is not an allowed value")
  }

  if(!ActPer[1] %in% c("N","Y")){
    stop("ActPer is not an allowed value")
  }

  if(!freqConversion[1] %in% c("ACT","SUM","AVG","END")){
    stop("freqConversion is not an allowed value")
  }

  if(!Alignment[1] %in% c("1ST","MID","END")){
    stop("Alignment is not an allowed value")
  }

  if(!Carry[1] %in% c("YES","NO","PAD")){
    stop("Carry is not an allowed value")
  }

  # Limit decimals a number in range to the range 0-9
  if(!is.numeric(Decimals)) Decimals <- 2L
  Decimals <- as.integer(Decimals)
  if(Decimals < 0) Decimals <- 0
  if(Decimals > 9) Decimals <- 9


  # Trim any excess for units
  Units <- substr(Units,0,12)

  # Replace any ISO currency codes with DS codes
  if(is.null(PrimeCurr)) {
    PrimeCurr <- ""
  }

  if(nchar(PrimeCurr) > 3){
    stop("Invalid currency.  Should be either 3 digit ISO code or Datastream code")
  } else if(nchar(PrimeCurr) == 3 ){
    # Check ISO code is valid and convert to DS Code
    dfXRef <- DatastreamDSWS2R::currencyDS2ISO
    if(PrimeCurr %in% dfXRef$isoCode){
      PrimeCurr <- dfXRef$dsCode[which(PrimeCurr == dfXRef$isoCode &
                                         dfXRef$primeCode == TRUE)]
    } else {
      stop("Invalid currency.  Should be an ISO code in table currencyDS2ISO.")
    }
  } else if(nchar(PrimeCurr) > 0 ){
    # Check DS Code is valid
    PrimeCurr <- iconv(PrimeCurr, from="utf-8", to = "latin1")
    dfXRef <- DatastreamDSWS2R::currencyDS2ISO
    if(!PrimeCurr %in% dfXRef$dsCode){
      stop("Invalid currency.  Should be an Datastream code in table currencyDS2ISO.")
    }
  }


  # At the moment everything will be a full update, and a hard coded NA value
  NA_VALUE <- "NA"


  # Add Start Date for values - make sure it is in DD/MM/YY format
  #CMC actually the function returns a dd/MM/yyyy format post Y2K
  # convert to xts object
  myXtsData <- as.xts(tsData)
  startDate <- zoo::index(first(myXtsData))
  endDate <- zoo::index(last(myXtsData))

  # Now create the URL to post the form to
  dsURL <- paste0(strServerName , strServerPage , "?UserID=" , strUsername)


  # Create a list of the parameters to be uploaded
  # We have not included the pair  AmendFlag="Y", so all these will be full updates

  dsParams <- list(CallType = "Upload",
                   TSMnemonic = toupper(TSCode),
                   TSMLM = toupper(MGMTGroup),
                   TSStartDate = format(startDate,format="%d/%m/%Y"),
                   TSEndDate = format(endDate,format="%d/%m/%Y"),
                   TSFrequency = freq[1],
                   TSTitle = seriesName,
                   TSUnits = Units,
                   TSDecPlaces = Decimals,
                   TSAsPerc = ActPer[1],
                   TSFreqConv = freqConversion[1],              # Add "Frequency Conversion"
                   TSAlignment = Alignment[1],                  # Add "Alignment"
                   TSCarryInd = Carry[1],                       # Add "Carry Indicator"
                   TSPrimeCurr = I(PrimeCurr),                  # Add "Prime Currency"
                   TSULCurr = "",                            # no longer use Underlying Currency, but need to pass up a null value as the mainframe is expecting it
                   ForceUpdateFlag1 = "Y",
                   ForceUpdateFlag2 = "Y",                   # We have ignored some logic in the original UCTS VBA code
                   #                   AmendFlag = "Y",
                   TSValsStart = format(startDate,format="%d/%m/%Y"),  #TODO adjust this date according to the frequency of the data VBA function AdjustDateTo1st
                   NAValue = NA_VALUE,
                   TSValues = .getTimeseries(myXtsData,
                                             freq= freq[1],
                                             digits=Decimals,
                                             NA_VALUE),           #Now add the datapoints - the date element of the series is discarded here, with obvious risks
                   UserOption = .EncryptPassword(strPassword)
  )


  # Now post the form
  # We will give it three tries
  nLoop <- 1
  waitTimeBase <- 2
  maxLoop <- 4
  retValue <- ""

  while(nLoop < maxLoop){
    retValue <- tryCatch(httr::POST(url = dsURL,
                                    body = dsParams,
                                    config =  httr::add_headers(encoding = "utf-8"),
                                    httr::content_type("application/x-www-form-urlencoded; charset=utf-8"),
                                    encode = "form"),
                         error = function(e) e)

    # Break if an error or null
    if(is.null(retValue)) break
    if("error" %in% class(retValue)) break

    # If did not get a time out then break
    if(httr::status_code(retValue) != 408) break

    # If not succesful then wait 2 seconds before re-submitting, ie give time for the
    # server/network to recover.
    Sys.sleep(waitTimeBase ^ nLoop)
    nLoop <- nLoop + 1
  }

  if(is.null(retValue)){
    return(structure(FALSE,
                     error = "NULL value returned"))
  }

  if("error" %in% class(retValue)){
    return(structure(FALSE,
                     error = paste("Error ", retValue$message)))
  }



  if(httr::http_error(retValue)){
    return(structure(FALSE,
                    error = paste("http Error: ", paste0(httr::http_status(retValue,
                                                                          collapse = " : ")))))
  }

  myResponse <- content(retValue, as = "text")

  if(myResponse[1] == "*OK*"){
    return(structure(TRUE,
                     error = ""))
  }
  else{
    return(structure(FALSE,
                     error = paste("*Error* Upload failed after ", nLoop,
                                   " attempts with error ", myResponse[1])))
  }
}



#' @title Append a xts to an existing UCTS timeseries in Datastream
#'
#' @description Uploads and appends an xts into a UCTS in the Datastream Database
#' @details This function checks if there is a pre-existing timeseries already in Datastream.
#' If there is then it will append the xts onto the existing series.  If there are any
#' overlapping dates then depending on the setting of overwrite then the new data
#' will overwrite the existing data in the UCTS
#'
#' @param tsData - an xts (or timeseries object that can be converted to
#' one) to be uploaded.
#' @param TSCode  The mnemonic of the target UCTS
#' @param MGMTGroup Must have managment group.  Only the first
#' characters will be used.
#' @param freq The frequency of the data to be uploaded
#' @param seriesName the name of the series
#' @param Units Units of the data - can be no more than 12 characters -
#'  excess will be trimmed to that length
#' @param Decimals Number of Decimals in the data - a number between 0 and
#'  9 - if outside that range then trimmed
#' @param ActPer Whether the values are percentages ("N") or actual
#' numbers ("Y")
#' @param freqConversion How to do any FX conversions
#' @param Alignment Alignment of the data within periods
#' @param Carry whether to carry data over missing dates
#' @param PrimeCurr the currency of the timeseries
#' @param overwrite if TRUE then existing data in the UCTS will be overwritten
#' @param strUsername your Datastream username
#' @param strPassword your Datastream Password
#' @param strServerName URL of the Datastream server
#' @param strServerPage page on the datastream server
#' @return TRUE if the upload has been a success, otherwise an error message
#'
#' @export
#'
#' @importFrom zoo index
#' @importFrom xts as.xts first last xtsible
#'
UCTSAppend <- function(tsData,
                       TSCode = "",
                       MGMTGroup = "ABC",
                       freq = c("D","W","M","Q","Y"),
                       seriesName,
                       Units = "",
                       Decimals = 2,
                       ActPer = c("N","Y"),
                       freqConversion = c("ACT","SUM","AVG","END"),
                       Alignment = c("1ST","MID","END"),
                       Carry = c("YES","NO","PAD"),
                       PrimeCurr ="",
                       overwrite = TRUE,
                       strUsername = ifelse(Sys.getenv("DatastreamUsername") != "",
                                            Sys.getenv("DatastreamUsername"),
                                            options()$Datastream.Username),
                       strPassword = ifelse(Sys.getenv("DatastreamPassword") != "",
                                            Sys.getenv("DatastreamPassword"),
                                            options()$Datastream.Password),
                       strServerName = "https://product.datastream.com",
                       strServerPage = "/UCTS/UCTSMaint.asp"){

  #Check inputs are valid - we can also rely on checks in UCTSUpload later

  if(!xtsible(tsData)){
    stop(paste0("tsData must be a time-based object and not of class ", class(tsData)))
  }
  tsData <- as.xts(tsData)

  if(!freq[1] %in% c("D","W","M","Q","Y")){
    stop("freq is not an allowed value")
  }

  # Get the existing UCTS from Datastream
  mydsws <- dsws$new()
  tsExisting <- mydsws$timeSeriesRequest(instrument = TSCode,
                                         startDate = as.Date("1950-01-01"),
                                         endDate = index(last(tsData)),
                                         frequency = freq)
  if(is.null(tsExisting)){
    errMsg <- paste0("Datastream Server Error retrieving existing series\n",
                     paste(mydsws$errorlist, collapse = "\n", sep = "\n"))
    stop(errMsg)
  }
  # In the absence of being able to define start and end dates for UCTS as defined
  # on http://product.datastream.com/DSWSClient/Docs/SoapApiHelp/EnumDetails.html#DSDateNames
  # We are going to trim the start and end of the series of any null values
  # If this is fixed by Datastream or another way is suggested then these lines
  # could be removed

  validRows <- which(!is.na(tsExisting))

  # Check if any data was found

  if(length(validRows) != 0){
    # There was no existing timeseries
    # Take the non-null middle segment

    firstNotNULL <- min(validRows)
    lastNotNULL <- max(validRows)

    tsExisting <- tsExisting[firstNotNULL:lastNotNULL, ]

    # Combine the new data with the existing data

    if(overwrite){
      # append with new data overwriting the old
      tsData <- xts::make.index.unique(rbind(tsData, tsExisting), drop = TRUE)

    } else {
      # append with old data being kept
      tsData <- xts::make.index.unique(rbind(tsExisting, tsData), drop = TRUE)
    }

  }

  # Upload combined timeseries


  return(UCTSUpload(tsData = tsData,
                    TSCode = TSCode,
                    MGMTGroup = MGMTGroup,
                    freq = freq,
                    seriesName = seriesName,
                    Units = Units,
                    Decimals = Decimals,
                    ActPer = ActPer,
                    freqConversion = freqConversion,
                    Alignment = Alignment,
                    Carry = Carry,
                    PrimeCurr = PrimeCurr,
                    strUsername = strUsername,
                    strPassword = strPassword,
                    strServerName = strServerName,
                    strServerPage = strServerPage))
}
