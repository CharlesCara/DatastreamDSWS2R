#' @include common.R
NULL

#' @title dsws
#'
#' @description An R5/RC object for accessing the Thomson Reuters Datastream DSWS service.
#'
#' @details Creates an R5/RC4 object for accessing the Thomson Reuters Datastream DSWS service
#'
#'
#' @export dsws
dsws <- setRefClass(Class="dsws",
                    fields = list(tokenList="ANY",
                                  serverURL = "character",
                                  username = "character",
                                  password = "character",
                                  initialised = "logical",
                                  errorlist = "ANY",
                                  requestList = "ANY",
                                  dataResponse = "ANY",
                                  myValues = "ANY",
                                  logging = "numeric"))

#-----------------------------------------------------------------------------

dsws$accessors(c("serverURL",
                 "username",
                 "password",
                 "errorlist",
                 "requestList",
                 "dataResponse"))


#-----------------------------------------------------------------------------

dsws$methods(initialize = function(dsws.serverURL = "", username = "", password = "", connect = TRUE){
  "initialises the class and unless noConnect is TRUE connects to the Datastream dsws server "

  .self$initialised <<- FALSE
  .self$errorlist <<- NULL

  if(dsws.serverURL == ""){
    .self$serverURL <<- "http://product.datastream.com/DSWSClient/V1/DSService.svc/rest/"
  } else {
    .self$serverURL <<- dsws.serverURL
  }

  if(username == ""){
    if(is.null(options()$Datastream.Username)){
      stop("Either username must be specified or it must be set via options(\"Datastream.Username\", \"Myusername\"")
    } else {
      .self$username <<- options()$Datastream.Username
    }
  } else {
    .self$username <<- username
  }

  if(password == ""){
    if(is.null(options()$Datastream.Password)){
      stop("Either username must be specified or it must be set via options(\"Datastream.Password\", \"Mypassword\"")
    } else {
      .self$password <<- options()$Datastream.Password
    }
  } else {
    .self$password <<- password
  }


  .self$tokenList <<- list(TokenValue = NULL,
                           TokenExpiry = NULL)
  .self$initialised <<- TRUE

  if(connect){
    .self$getToken()
  }

  return(invisible(.self))
})





#-----------------------------------------------------------------------------
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL
dsws$methods(getToken = function(){
  "Returns a Token from the the dsws server that gives permission to access data"
  if(!.self$initialised) {
    message("dsws has not been properly initialised.  Check serverURL, username and password")
    return(NULL)
  }

  ts <- .self$tokenList
  if(is.null(ts$TokenValue) || is.null(ts$TokenExpiry) || ts$TokenExpiry < Sys.time()){


    myTokenURL <- paste0(.self$serverURL, "Token",
                         "?username=", .self$username ,
                         "&password=", .self$password )


    myTokenResponse <- tryCatch(RCurl::getURL(url = myTokenURL),
                                error = function(e) {
                                  message(e)
                                  stop("Could not request access Token")
                                  return(NULL)})
    if(!is.null(myTokenResponse)){
      myTokenList <- rjson::fromJSON(myTokenResponse)
      #TODO: Error check response
      .self$tokenList <<- list(TokenValue = myTokenList$TokenValue,
                               TokenExpiry = .convert_JSON_Datetime(myTokenList$TokenExpiry))
    } else {
      stop("Could not request access Token - response from server was NULL")
    }
  }

  return(invisible(.self$tokenList$TokenValue))
})



#-----------------------------------------------------------------------------
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
dsws$methods(makeRequest = function(){
  "make a request from the DSWS server.  The request (in a R list form) is taken from
  .self$requestList, parsed into JSON and sent to the DSWS server.  The JSON response
  is parsed and saved in .self$dataResponse"


  myDataURL <- paste0(.self$serverURL , "GetData")

  myRequestJSON <- rjson::toJSON(.self$requestList)

  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")

  myDataResponse <- tryCatch({
    RCurl::postForm(myDataURL,
                    .opts=list(httpheader=httpheader
                               ,postfields=myRequestJSON))},
    error = function(e) {
      message(e)
      .self$errors <<- e
      return(FALSE)})

  if(is.null(myDataResponse)){
    myData <- NULL
  } else {
    myData <- rjson::fromJSON(json_str = myDataResponse)
  }

  .self$dataResponse <- myData

  return(TRUE)

})


#-----------------------------------------------------------------------------
dsws$methods(listRequest = function(instrument,
                                    datatype = "",
                                    expression = "",
                                    requestDate){
  "Return a listRequest from Datastream dsws.  instrument should contain a list mnemonic,
  such as 'LFTSE100'  "

  return(.self$basicRequest(instrument = instrument,
                            datatype = datatype,
                            expression = expression,
                            isList = TRUE,
                            startDate = "",
                            endDate = requestDate,
                            frequency = "",
                            kind = 0,
                            format = format))
})


#-----------------------------------------------------------------------------
dsws$methods(snapshotRequest = function(instrument,
                                        datatype = "",
                                        expression = "",
                                        requestDate){
  "Return a snapshot (static) Request from Datastream dsws"

  return(.self$basicRequest(instrument = instrument,
                            datatype = datatype,
                            expression = expression,
                            isList = FALSE,
                            startDate = "",
                            endDate = requestDate,
                            frequency = "D",
                            kind = 0,
                            format = "Snapshot"))
})


#-----------------------------------------------------------------------------
dsws$methods(timeSeriesRequest = function(instrument,
                                          datatype = "",
                                          expression = "",
                                          startDate,
                                          endDate,
                                          frequency = "D",
                                          format = c("ByInstrument","ByDatatype")){
  "Return a timeSeriesRequest from Datastream dsws.  Should request either a datatype or an expression
  not both.  If a datatype is provided then anythink in Expression will be ignored"

  myData <- .self$basicRequest(instrument = instrument,
                               datatype = datatype,
                               expression = expression,
                               isList = FALSE,
                               startDate = startDate,
                               endDate = endDate,
                               frequency = frequency,
                               kind = 1,
                               format = format)
  return(myData)

})


#-----------------------------------------------------------------------------
dsws$methods(timeSeriesListRequest = function(instrument,
                                              datatype = "",
                                              expression = "",
                                              startDate,
                                              endDate,
                                              frequency = "D",
                                              format = c("ByInstrument","ByDatatype")){
  "Return a timeSeriesListRequest from Datastream dsws"

  return(.self$basicRequest(instrument = instrument,
                            datatype = datatype,
                            expression = expression,
                            isList = FALSE,
                            startDate = startDate,
                            endDate = endDate,
                            frequency = frequency,
                            kind = 1,
                            format = format))
})

#-----------------------------------------------------------------------------
dsws$methods(basicRequest = function(instrument,
                                     datatype = "",
                                     expression = "",
                                     isList = FALSE,
                                     startDate,
                                     endDate,
                                     frequency = "D",
                                     kind = 0,
                                     format = "ByInstrument"){

  "Return a timeSeriesRequest from Datastream dsws.  Should request either a datatype or an expression
  not both.  If a datatype is provided then anythink in Expression will be ignored"

  .self$setErrorlist(NULL)

  # Check inputs
  if(!frequency %in% c("D", "W", "M", "Q", "Y")){
    stop("frequency must be one of D, W, M, Q or Y")
  }

  # Only use expressions if datatype is blank.  Expression has to be substituted into instrument
  numInstrument <- length(instrument)
  instrument<-toupper(instrument)

  if(datatype == "" && expression != ""){
    numDatatype <- 1
    isDatatype <- FALSE
    if( grepl(pattern="XXXX", x=expression, fixed=TRUE) == FALSE){
      # Expression does not contain XXXX so we cannot do a replace
      stop("Expressions must contain XXXX so that they can be inserted into instrument")
    } else {
      # convert instrument by inserting instruments into the Expression
      instrument <- sapply(instrument,
                           FUN=function(x) gsub(pattern="XXXX",replacement=x,x=expression,fixed=TRUE),
                           USE.NAMES = FALSE)

    }
    myDataType <- list()

  } else {
    numDatatype <- length(datatype)
    isDatatype <- TRUE
    myDataType <- lapply(datatype, FUN = function(x) return(list(Properties= NULL, Value = x)))
  }

  # Limit of size of requests
  if(numInstrument * numDatatype >= 2000) {
    stop("Maximum number of dataitems is in excess of 2000.  Chunked requests not yet implmented")
  }

  # If we are making a list request then need to set IsList to be TRUE
  if(isList){
    instrumentProperties <- list(list(Key = "IsList", Value = TRUE))
  } else {
    instrumentProperties <- NULL
  }


  # If more than one instrument then we have to pass IsSymbolSet=TRUE to wsds
  if(numInstrument > 1){
    myInstrument <- list(Properties = list(list(Key = "IsSymbolSet",
                                                Value  = TRUE)),
                         Value = paste0(instrument, collapse=","))
  } else {
    myInstrument <- list(Properties= instrumentProperties,
                         Value = instrument)
  }


  # Set up the Date element with type checking
  if(class(startDate)[1] %in% c("Date", "POSIXct", "POSIXt")){
    sStartDate <- format(startDate, "%Y-%m-%d")
  } else if(class(startDate) == "character"){
    sStartDate <- startDate
  } else {
    stop("startDate should be either a valid character string or a Date (class either Date, POSIXct, POSIXlt)")
  }

  if(class(endDate)[1] %in% c("Date", "POSIXct", "POSIXt")){
    sEndDate <- format(endDate, "%Y-%m-%d")
  } else if(class(endDate) == "character"){
    sEndDate <- endDate
  } else {
    stop("startDate should be either a valid character string or a Date (class either Date, POSIXct, POSIXlt)")
  }


  myDates <- list(End = sEndDate,
                  Frequency = frequency,
                  Kind = kind,
                  Start = sStartDate)

  # Combine all these elements to create the request (in list form)
  .self$requestList <- list(DataRequest = list(DataTypes = myDataType,
                                               Date = myDates,
                                               Instrument = myInstrument,
                                               Tag = NULL),
                            Properties = list(Properties=NULL),
                            TokenValue = .self$getToken())


  ret <- .self$makeRequest()

  if(!ret){
    # There has been an error.  Return NULL.  Error is stored in .self$errors
    return(NULL)
  }

  # Now to parse the timeseries data in myData into an xts
  # If we have more than one dimension then it is returned as a list of wide xts

  # Get the dates - these are provided separately
  myDates <- .convert_JSON_Date(dataResponse$DataResponse$Dates)

  if(length(.convert_JSON_Date(dataResponse$DataResponse$Dates)) == 0 ){
    # If the length of the Dates object is 0 then no data has been returned
    # return a NULL xts
    return(xts(NULL))
  }

  xtsData <- list()

  if(format[1] == "ByInstrument"){
    # If the format is byInstrument, then we are going to create a list of wide xts, one for each datatype

    #    if(isDatatype){
    # We have sent the request as multiple instruments and multiple datatypes so
    # response has a single item in DataTypeValues
    for(iDatatype in 1:numDatatype){

      # Create a dataframe to hold the results
      .self$myValues <- data.frame(matrix(NA, nrow = length(myDates), ncol = numInstrument))

      # Place the returned data into columns of the dataframe and name the column
      for(iInstrument in 1:numInstrument){
        .self$parseBranch(iInstrument,
                          iDatatype,
                          formatType = "ByInstrument")
      }

      # Turn it into a xts and if more than one datatype was requested put it into a list
      # We could in future save the xts into an environment as well  - a la Quantmod package
      if(numDatatype == 1){
        xtsData <- xts(.self$myValues, order.by = myDates)
      } else {
        xtsData[[iDatatype]] <- xts(.self$myValues, order.by = myDates)
      }
    }

  } else if(format == "ByDatatype"){
    # If the format is byDatatype, then we are going to create a list of wide xts, one for each instrument
    # this is closer to the getSymbols function of the quantmod package and so might be a springboard
    # for extending to that package
    for(iInstrument in 1:numInstrument){

      # Create a dataframe to hold the results
      .self$myValues <- data.frame(matrix(NA, nrow = length(myDates), ncol = numDatatype))

      # Place the returned data into columns of the dataframe and name the column
      for(iDatatype in 1:numDatatype){
        .self$parseBranch(iInstrument,
                          iDatatype,
                          formatType = "ByInstrument")
      }

      # Turn it into a xts and if more than one datatype was requested put it into a list
      if(numInstrument == 1){
        xtsData <- xts(.self$myValues, order.by = myDates)
      } else {
        xtsData[[iInstrument]] <- xts(.self$myValues, order.by = myDates)
      }
    }

  } else if(format == "Snapshot") {
    # Process the response into a dataframe, one row per instrument, with a column for each datatype
    # Create a dataframe to hold the results
    .self$myValues <- data.frame(matrix(NA, nrow = numInstrument, ncol = numDatatype + 1))
    colnames(.self$myValues)[1] <- "Instrument"
    for(iDatatype in 1:numDatatype){
      # Put a title on the column
      colnames(.self$myValues)[iDatatype + 1] <-
        .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$DataType
      for(iInstrument in 1:numInstrument){
      .self$parseSnapshotBranch(iInstrument,
                                iDatatype)
        }
      }
      return(.self$myValues)
  } else {
    stop("Output format is not ByDatatype or ByInstrument")
  }
  return(xtsData)
})


dsws$methods(parseBranch = function(iInstrument, iDatatype, formatType){

  # we are using eval to avoid copying what might be a big table of in myValues
  myValuesList <- .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Value

  myValuesList[sapply(myValuesList, is.null)] <- NA

  if(!(TRUE %in% grepl("[$$ER:]", myValuesList[[1]]) )){
      .self$myValues[,iInstrument] <- t(data.frame(lapply(myValuesList, FUN = .convertJSONString)))
  }

  # Add column names
    colnames(.self$myValues)[iInstrument] <-
      .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Symbol
  # Replace errors with NA
    .self$myValues[which(myValues[,iDatatype] == "$$ER: 0904,NO DATA AVAILABLE"),iDatatype] <- NA

  return(NULL)

})


dsws$methods(parseSnapshotBranch = function(iInstrument, iDatatype){

  # we are using eval to avoid copying what might be a big table of in myValues
  myValuesList <- .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Value

  myValuesList[sapply(myValuesList, is.null)] <- NA

  if(!(TRUE %in% grepl("[$$ER:]", myValuesList[[1]]) )){
    .self$myValues[iInstrument, iDatatype + 1] <- .convertJSONString(myValuesList)
  }

  # Add row names
  .self$myValues[iInstrument, 1] <-
    .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Symbol
  # Replace errors with NA
  .self$myValues[which(myValues[,iDatatype + 1] == "$$ER: 0904,NO DATA AVAILABLE"), iDatatype + 1] <- NA

  return(NULL)

})





