#' @include common.R
#' @include classConstructor.R
#' @include wrapper.R
#'

#' @name dotgetTimeseries
#' @title convert xts timeseries into a string that can be sent to
#' the Datastream server.  This strips out weekends from the timeseries
#'
#' @param Data the xts timeseries to be converted
#' @param freq the frequency of the data
#' @param digits the number of decimal places to round the data to
#'
#' @return A string of the core data of Data
#'
#'
#' @importFrom zoo zoo index
#' @importFrom xts merge.xts .indexwday
#' @keywords internal
#' @noRd
#'
.getTimeseries <- function(Data, freq, digits) {
  if (ncol(Data) > 1) {
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
    NADates <- seq(from = startDate, to = endDate, by = "days")
    NAData <- zoo(c(NA), order.by = NADates)
    #merge and fill missing rows with NAs
    wData <- xts::merge.xts(Data, NAData, fill = NA)

    # This only picks the weeksdays from the original series
    wData <- wData[which(xts::.indexwday(wData) %in% 1:5),1]
  }else{
    wData <- Data
    #If we do not have a daily frequency then we can just load up the datapoints, with the implicit
    #assumption that they are in the right frequency
  }

  # Convert to numeric
  suppressWarnings({
    wData <- as.numeric(zoo::coredata(wData)[, 1])
    wData[which(!is.finite(wData))] <- as.numeric(NA)
    wData <- round(wData, digits = digits)

  })
  return(I(wData))
}



# UserObjectType values
USEROBJECTTYPE_LIST <- 1
USEROBJECTTYPE_INDEX <- 2
USEROBJECTTYPE_TIMESERIES <- 3
USEROBJECTTYPE_EXPRESSION <- 4


# ShareType Values
SHARETYPE_DEFAULT <- 0

# CarryIndicator Values
CARRY_VALUES = c("YES" = 0,"NO" = 1,"PAD" = 2)

# DateAlignment Values
ALIGNMENT_VALUES = c("1ST" = 1, "FIRST" = 1, "MID" = 2,"END" = 0)

# FrequencyConversion Values
FREQCONV_VALUES = c("ACT" = 3,"SUM" = 2,"AVG" = 1,"END" = 0)

# Data frequency values
DATAFREQUENCY_VALUES = c("D" = 0, "W" = 1, "M" = 2, "Q" = 3, "Y" = 4)

# Whether Percentage or not
ACTPER_VALUES <- c("N" = FALSE, "Y" = TRUE)

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
#' @param seriesName the name of the series - can be no more than XX characters -
#'  excess will be trimmed to that length
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
#' @param strUsername Deprecated will be removed in a future release - ignored
#' @param strPassword Deprecated will be removed in a future release - ignored
#' @param mydsws a dsws connection object
#' @param strServerName URL of the Datastream server
#' @param strServerPage page on the datastream server
#' @return TRUE if the upload has been a success, FALSE with attribute error containing the error message
#'
#' @export
#'
#' @importFrom zoo index
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
                       strUsername = NULL,
                       strPassword = NULL,
                       mydsws = dsws$new(),
                       strServerName="https://product.datastream.com",
                       strServerPage="/dswsclient/V1/DSUserDataService.svc/rest/UpdateItem") {

  #Check inputs are valid

  if (!xtsible(tsData)) {
    stop(paste0("tsData must be a time-based object and not of class ",class(tsData)))
  }

  if (!freq[1] %in% names(DATAFREQUENCY_VALUES)) {
    stop(paste0("freq is not an allowed value. ", freq[1], " must be one of ", paste0(names(DATAFREQUENCY_VALUES), sep = ", ")))
  }

  if (!ActPer[1] %in% names(ACTPER_VALUES)) {
    stop("ActPer is not an allowed value")
  }

  if (!freqConversion[1] %in% names(FREQCONV_VALUES)) {
    stop("freqConversion is not an allowed value")
  }

  if (!Alignment[1] %in% names(ALIGNMENT_VALUES)) {
    stop("Alignment is not an allowed value")
  }

  if (!Carry[1] %in% names(CARRY_VALUES)) {
    stop(paste0("Carry is not an allowed value. ", Carry[1], " must be one of ", paste0(names(CARRY_VALUES), sep = ", ")))
  }

  # Limit decimals a number in range to the range 0-9
  if (!is.numeric(Decimals)) Decimals <- 2L
  Decimals <- as.integer(Decimals)
  if (Decimals < 0) Decimals <- 0
  if (Decimals > 9) Decimals <- 9


  # Trim any excess for units
  Units <- substr(Units,0,12)

  # Replace any ISO currency codes with DS codes
  if (is.null(PrimeCurr)) {
    PrimeCurr <- ""
  }

  if (nchar(PrimeCurr) > 3) {
    stop("Invalid currency.  Should be either 3 digit ISO code or Datastream code")
  } else if (nchar(PrimeCurr) == 3 ) {
    # Check ISO code is valid and convert to DS Code
    dfXRef <- DatastreamDSWS2R::currencyDS2ISO
    if (PrimeCurr %in% dfXRef$isoCode) {
      PrimeCurr <- dfXRef$dsCode[which(PrimeCurr == dfXRef$isoCode &
                                         dfXRef$primeCode == TRUE)]
    } else {
      stop("Invalid currency.  Should be an ISO code in table currencyDS2ISO.")
    }
  } else if (nchar(PrimeCurr) > 0 ) {
    # Check DS Code is valid
    PrimeCurr <- iconv(PrimeCurr, from = "utf-8", to = "latin1")
    dfXRef <- DatastreamDSWS2R::currencyDS2ISO
    if (!PrimeCurr %in% dfXRef$dsCode) {
      stop("Invalid currency.  Should be an Datastream code in table currencyDS2ISO.")
    }
  }


  # convert to xts object
  myXtsData <- xts::as.xts(tsData)

  # If we are using Daily data and the first day falls on a weekend then move that date to Friday

  if (freq[1] == "D") {
    startDay <- xts::.indexwday(myXtsData[1,])
    if (startDay == 6) {
      zoo::index(myXtsData)[1] <- zoo::index(myXtsData)[1] - 1
    } else if (startDay == 0) {
      zoo::index(myXtsData)[1] <- zoo::index(myXtsData)[1] - 2
    }
  }

  # Start and end date for the dataInput item
  startDate <- zoo::index(first(myXtsData))
  endDate <- zoo::index(last(myXtsData))

  dataInputClass <- list(EndDate = .js_date_jsonstring(endDate),
                         Frequency = unname(DATAFREQUENCY_VALUES[freq[1]]),
                         StartDate = .js_date_jsonstring(startDate),
                         Values = .getTimeseries(myXtsData,
                                                 freq = freq[1],
                                                 digits = Decimals))

  UserObject = list("__type" = "DSTimeSeriesRequestObject:http://dsws.datastream.com/client/V1/",
                    Created = .js_date_jsonstring(Sys.time()),
                    AccessRight = 0,
                    Description = seriesName,
                    DisplayName = seriesName,
                    Id = TSCode,
                    LastModified = .js_date_jsonstring(Sys.time()),
                    Mnemonic = TSCode,  # set to be the same as the id
                    Owner = NULL, # not being set at the moment
                    ShareType = SHARETYPE_DEFAULT,
                    AsPercentage = unname(ACTPER_VALUES[ActPer[1]]), # This is a decrepated property
                    CarryIndicator = unname(CARRY_VALUES[Carry[1]]),
                    DateAlignment = unname(ALIGNMENT_VALUES[Alignment[1]]),
                    DecimalPlaces = Decimals,
                    FrequencyConversion = unname(FREQCONV_VALUES[freqConversion[1]]),
                    HasPadding = FALSE, # This is a decrepated property
                    ManagementGroup = MGMTGroup,
                    PrimeCurrencyCode = PrimeCurr,
                    UnderCurrencyCode = "", # This is a decrepated property
                    Units = Units,
                    DataInput = dataInputClass)

  # Now create the URL to post the form to
  myDataURL <- paste0(strServerName , strServerPage)

  return(processUpload(UserObject = UserObject,
                       UserObjectType = USEROBJECTTYPE_TIMESERIES,
                       URL = myDataURL,
                       mydsws = mydsws))
}


#' Process a user object which might be a timeseries or an index
#' @importFrom httr2 request req_body_json req_retry req_perform resp_is_error resp_body_json
processUpload <- function(UserObject, UserObjectType, URL, mydsws) {

  # Get the token from the dsws object or create a new one
  if (inherits(mydsws, "dsws")) {
    tokenValue <- mydsws$tokenList$TokenValue
  } else {
    tokenValue <- DatastreamDSWS2R::dsws$new()$tokenList$TokenValue
  }


  dsRequest <- list(UserObject = UserObject,
                    Properties = NULL,
                    Filters = NULL,
                    TokenValue = tokenValue,
                    "UserObjectType" = UserObjectType)


  httr2::request(URL) |>
    httr2::req_headers(accept = "application/json",
                       encode = "json") |>
    httr2::req_body_json(dsRequest, na = "null") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() ->
    response

  if (httr2::resp_is_error(response)) {
    return(structure(FALSE,
                     error = httr2::resp_status_desc(response)))
  }

  # If we have an invalid request then we need to return the message
  # returned as a string
  if (httr2::resp_content_type(response) == "text/html") {
    return(structure(FALSE,
                     error = httr2::resp_body_string(response)))
  }


  # Success code
  if (httr2::resp_body_json(response)$ResponseStatus != 0 ) {
    return(structure(FALSE,
                     error = httr2::resp_body_json(response)$ErrorMessage))
  }

  return(structure(TRUE,
                   error = ""))

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
#' @param mydsws a dsws object that can be passed in.  Use this to avoid creating another dsws
#' object in the same session.
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
                       mydsws = dsws$new(),
                       strUsername = NULL,
                       strPassword = NULL,
                       strServerName = "https://product.datastream.com",
                       strServerPage = "/dswsclient/V1/DSUserDataService.svc/rest/UpdateItem") {
  .Deprecated("UCTSAppend is deprecated and will be removed in a future release.")

  #Check inputs are valid - we can also rely on checks in UCTSUpload later

  if (!xtsible(tsData)) {
    stop(paste0("tsData must be a time-based object and not of class ", class(tsData)))
  }
  tsData <- as.xts(tsData)

  if (!freq[1] %in% c("D","W","M","Q","Y")) {
    stop("freq is not an allowed value")
  }

  # Get the existing UCTS from Datastream
  if (is.null(mydsws)) {
    mydsws <- dsws$new()
  }
  tsExisting <- mydsws$timeSeriesRequest(instrument = TSCode,
                                         startDate = as.Date("1950-01-01"),
                                         endDate = index(last(tsData)),
                                         frequency = freq)
  if (is.null(tsExisting)) {
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

  if (length(validRows) != 0) {
    # There was no existing timeseries
    # Take the non-null middle segment

    firstNotNULL <- min(validRows)
    lastNotNULL <- max(validRows)

    tsExisting <- tsExisting[firstNotNULL:lastNotNULL, ]

    # Combine the new data with the existing data

    if (overwrite) {
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
                    mydsws = mydsws,
                    strServerName = strServerName,
                    strServerPage = strServerPage))
}
