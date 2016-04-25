#' @include common.R
#' @include classConstructor.R
#' @include wrapper.R
#'
#' @title Encrypt the Datastream password
#' @description This is a port of the VBA code
#'
#' @param strPassword the password to be encrypted
#' @return an encrypted password
#'
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
#' @importFrom RCurl postForm curlPercentEncode
#' @importFrom xts as.xts first last
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
                       strUsername=options()$Datastream.Username,
                       strPassword=options()$Datastream.Password,
                       strServerName="http://product.datastream.com",
                       strServerPage="/UCTS/UCTSMaint.asp"){

  #Check inputs are valid
  if(!freq %in% c("D","W","M","Q","Y")){
    stop("freq is not an allowed value")
  }

  if(!ActPer %in% c("N","Y")){
    stop("ActPer is not an allowed value")
  }

  if(!freqConversion %in% c("ACT","SUM","AVG","END")){
    stop("freqConversion is not an allowed value")
  }

  if(!Alignment %in% c("1ST","MID","END")){
    stop("Alignment is not an allowed value")
  }

  if(!Carry %in% c("YES","NO","PAD")){
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
  if(nchar(PrimeCurr) > 3){
    stop("Invalid currency.  Should be either 3 digit ISO code or Datastream code")
  } else if(nchar(PrimeCurr) == 3 ){
    # Check ISO code is valid and convert to DS Code
    data("currencyDS2ISO")
    if(PrimeCurr %in% currencyDS2ISO$isoCode){
      PrimeCurr <- currencyDS2ISO$dsCode[which(PrimeCurr == currencyDS2ISO$isoCode &
                                                 currencyDS2ISO$primeCode == TRUE)]
    } else {
      stop("Invalid currency.  Should be an ISO code in table currencyDS2ISO.")
    }
  } else if(nchar(PrimeCurr) > 0 ){
    # Check DS Code is valid
    data("currencyDS2ISO")
    if(!PrimeCurr %in% currencyDS2ISO$dsCode){
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
                   TSFrequency = freq,
                   TSTitle = seriesName,
                   TSUnits = Units,
                   TSDecPlaces = Decimals,
                   TSAsPerc = ActPer,
                   TSFreqConv = freqConversion,              # Add "Frequency Conversion"
                   TSAlignment = Alignment,                  # Add "Alignment"
                   TSCarryInd = Carry,                       # Add "Carry Indicator"
                   TSPrimeCurr = enc2native(PrimeCurr),                  # Add "Prime Currency" in native (single byte encoding)
                   TSULCurr = "",                            # no longer use Underlying Currency, but need to pass up a null value as the mainframe is expecting it
                   ForceUpdateFlag1 = "Y",
                   ForceUpdateFlag2 = "Y",                   # We have ignored some logic in the original UCTS VBA code
                   TSValsStart = format(startDate,format="%d/%m/%Y"),  #TODO adjust this date according to the frequency of the data VBA function AdjustDateTo1st
                   NAValue = NA_VALUE,
                   TSValues = .getTimeseries(myXtsData,
                                          freq= freq,
                                          digits=Decimals,
                                          NA_VALUE),           #Now add the datapoints - the date element of the series is discarded here, with obvious risks
                   UserOption = .EncryptPassword(strPassword)
  )


  # Now post the form
  # We will give it three tries
  iCounter <- 1
  retValue <- ""

  while(iCounter <3 && retValue != "*OK*"){
    retValue <- RCurl::postForm(uri = dsURL,
                         .params = dsParams,
                         style = "POST",
                         .encoding = "utf-8",
                         .contentEncodeFun = RCurl::curlPercentEncode)
    if(retValue != "*OK*"){
      # If not succesful then wait 2 seconds before re-submitting, ie give time for the
      # server/network to recover.
      Sys.sleep(time = 2)
    }
    iCounter <- iCounter + 1
  }
  if(retValue[1] == "*OK*"){
    return(TRUE)
  }
  else{
    return(paste("*Error* Upload failed after ", iCounter, " attempts with error ", retValue[1]))
  }
}
