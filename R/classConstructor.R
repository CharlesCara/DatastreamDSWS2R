#' @include common.R
NULL

#' @title dsws
#'
#' @description An R5/RC object for accessing the Thomson Reuters Datastream
#'   DSWS service.
#'
#' @details Creates an R5/RC4 object for accessing the Thomson Reuters
#'   Datastream DSWS service
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
                                  symbolList = "ANY",
                                  myValues = "ANY",
                                  myTypes = "ANY",
                                  logging = "numeric",
                                  numDatatype = "numeric",
                                  numInstrument = "numeric",
                                  chunkLimit = "numeric"))

#-----Accessors----------------------------------------------------------------

dsws$accessors(c("serverURL",
                 "username",
                 "password",
                 "logging",
                 "errorlist",
                 "requestList",
                 "dataResponse",
                 "symbolList"))


#------Initialisation-----------------------------------------------------------------------

dsws$methods(initialize = function(dsws.serverURL = "", username = "", password = "", connect = TRUE){
  "
  initialises the class.
  Unless noConnect is TRUE also connects to the
  Datastream dsws server.  If the username and password are not
  provided, then they are sourced from
      options()$Datastream.Username and
      options()$Datastream.Password

    This allows the password to be stored in .RProfile rather
  than in the source code.
  "

  .self$initialised <<- FALSE
  .self$errorlist <<- list()
  .self$chunkLimit <<- 2000L   # Max number of items that can be in a single request.  Set by Datastream
  .self$logging <<- 0L

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
    .self$.getToken()
  }

  return(invisible(.self))
})





#------getToken-----------------------------------------------------------------------
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL
dsws$methods(.getToken = function(){
  "Internal function:
   Returns a Token from the the dsws server that
  gives permission to access data."
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
dsws$methods(.makeRequest = function(){
  "Internal function: make a request from the DSWS server.
  The request  (in a R list form) is taken from .self$requestList,
  parsed into JSON and sent to the DSWS server.  The JSON response is
  parsed and saved in .self$dataResponse"


  myDataURL <- paste0(.self$serverURL , "GetData")

  myRequestJSON <- rjson::toJSON(.self$requestList)

  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")

  if(.self$logging >=3 ){
    startTime <- Sys.time()
  }

  if(.self$logging >=5 ){
    message(paste0("JSON request to DSWS server response is:\n", myRequestJSON))
  }

  myDataResponse <- tryCatch({
    RCurl::postForm(myDataURL,
                    .opts=list(httpheader=httpheader
                               ,postfields=myRequestJSON))
  },
  error = function(e) {
    message(e)
    .self$errors <<- e
    return(FALSE)})

  if(.self$logging >=3 ){
    message(paste0("DSWS server request took ", Sys.time() - startTime))
  }

  if(.self$logging >=5 ){
    message(paste0("DSWS server response is:\n", myDataResponse))
  }

  if(is.null(myDataResponse)){
    .self$dataResponse <-  NULL
  } else {
    if(.self$logging >=3 ){
      startTime <- Sys.time()
    }
    .self$dataResponse <- rjson::fromJSON(json_str = myDataResponse)

    if(.self$logging >=3 ){
      message(paste0("Processing response into JSON took ", Sys.time() - startTime))
    }

  }

  if(.self$logging >= 4 ){
    message(paste0("JSON returned by DSWS server response is:\n", .self$dataResponse))
  }

  return(TRUE)

})


#----------------------------------------------------------------------------
dsws$methods(listRequest = function(instrument,
                                    datatype = "",
                                    expression = "",
                                    requestDate){
  "
  Make a listRequest from Datastream DSWS.
  This is the equivalent to
  the Excel static request for a list.\n
  Parameters are: \n
      instrument -- should contain a list mnemonic, such as 'LFTSE100'
      Can be a user created list or index.  The UCL can contain
      expressions.\n
      datatype -- array of datatypes eg NAME, MNEM, P, PE etc\n
      expression -- if datatype is null or '' then an expression
      eg PCH#(XXXX,3M)\n
      requestDate -- either a Date or a string with a datastream
      relative date eg '-3M'\n

  Returns a data.frame with the requested data.\n

  Examples:\n

      mydsws$listRequest(instrument = \"LFTSE100\", \n
      datatype = c(\"NAME\",\"P\"), \nrequestDate = \"-0D\")

      mydsws$listRequest(instrument = \"LFTSE100\", \n
      expression = \"PCH#(XXXX,3M)\", requestDate = Sys.Date())

  "
  return(.self$.basicRequest(instrument = instrument,
                             datatype = datatype,
                             expression = expression,
                             isList = TRUE,
                             startDate = "",
                             endDate = requestDate,
                             frequency = "D",
                             kind = 0,
                             format = "SnapshotList"))
})


#-----------------------------------------------------------------------------
dsws$methods(snapshotRequest = function(instrument,
                                        datatype = "",
                                        expression = "",
                                        requestDate){
  "
  Make a snapshotRequest from Datastream DSWS.
  This is the equivalent
  to the Excel static request for an array of instruments.\n
  Parameters are: \n
  instrument -- should one or more instruments eg \"MKS\" or\n
      c(\"MKS\",\"@AAPL\").  The array can contain
      Economics codes and Expressions.
  datatype -- array of datatypes eg NAME, MNEM, P, PE etc\n
  expression -- if datatype is null or '' then an expression \n
      eg PCH#(XXXX,3M)\n
  requestDate -- either a Date or a string with a datastream relative\n
      date eg '-3M'\n

  Returns a data.frame with the requested data.\n

  Examples:\n

  mydsws$snapshotRequest(instrument = c(\"MKS\",\"@AAPL\"), \n
   datatype = c(\"NAME\",\"P\"), requestDate = \"-0D\")

  mydsws$snapshotRequest(instrument = c(\"MKS\",\"@AAPL\"), \n
   expression = \"PCH#(XXXX,3M)\", requestDate = \"-0D\")

  "

  return(.self$.basicRequest(instrument = instrument,
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
                                          format = "ByInstrument"){

  "
  Return a timeSeriesRequest from Datastream dsws.
  Should request either
  a datatype or an expression
  not both.  If a datatype is provided then anythink in Expression
  will be ignored

  Make a timeSeriesRequest from Datastream DSWS.  This is the equivalent
  to the Excel timeseries request for an array of instruments.\n
  Parameters are: \n
  instrument -- should one or more instruments eg \"MKS\" or \n
   c(\"MKS\",\"@AAPL\").  The array can contain Economics codes and
     Expressions.
  datatype -- array of datatypes eg P, PE etc\n
  expression -- if datatype is null or '' then an expression
  eg PCH#(XXXX,3M)\n
  startDate -- either a Date or a string with a datastream relative
  date eg '-3M'\n
  endDate -- either a Date or a string with a datastream relative
  date eg '-0D'\n
  frequency -- one of the standard Datastream
    frequencies - D, W, M, Q, or Y
  format -- can be either  \"ByInstrument\" or \"ByDatatype\".

  Returns either a single xts or a list of xts a data.frame with
  the requested data.  If \"ByInstrument\" then
  the data is returned as one or more (ie a list) wide xts with one
  column per instrument.  If \"ByDatatype\"
  then the data is returned as one or more (ie a list) of wide xts with
  one column per Datatype.  This format
  is more compatible with the quantmod package.

  Examples:\n

  mydsws$timeSeriesRequest(instrument = c(\"MKS\",\"@AAPL\"),\n
      datatype = \"P\", startDate = \"-30D\",\n
      endDate = \"-0D\", frequency = \"D\")

  mydsws$timeSeriesRequest(instrument = c(\"MKS\"), \n
      expression = \"PCH#(XXXX,3M)\", startDate = \"-30D\",\n
      endDate = \"-0D\", frequency = \"D\")

  mydsws$timeSeriesRequest(instrument = c(\"MKS\",\"@AAPL\"), \n
      datatype = (\"P\",\"UP\"), startDate = \"-30D\",\n
      endDate = \"-0D\", frequency = \"D\", format = \"ByDatatype\")

  "
  if(.self$logging >=3 ){
    overallStartTime <- Sys.time()
  }
  myData <- .self$.basicRequest(instrument = instrument,
                                datatype = datatype,
                                expression = expression,
                                isList = FALSE,
                                startDate = startDate,
                                endDate = endDate,
                                frequency = frequency,
                                kind = 1,
                                format = format)
  if(.self$logging >=3 ){
    message(paste0("Processing request took ", Sys.time() - overallStartTime))
  }
  return(myData)

})


#-----------------------------------------------------------------------------
dsws$methods(timeSeriesListRequest = function(instrument,
                                              datatype = "",
                                              expression = "",
                                              startDate,
                                              endDate,
                                              frequency = "D",
                                              format = "ByInstrument"){

  "
  Make a timeSeriesListRequest from Datastream DSWS.
  This is the
  equivalent to the Excel timeseries request for an array of instruments.
  Should request either a datatype or an expression not both.  If a
  datatype is provided then anything in Expression will be ignored.\n
  Parameters are: \n
  instrument -- should contain a list mnemonic, such as \"LFTSE100\"\n.
  Can be a user created list or index.  The UCL can contain expressions.
  datatype -- array of datatypes eg P, PE etc\n
  expression -- if datatype is null or '' then an expression \n
    eg PCH#(XXXX,3M)\n
  startDate -- either a Date or a string with a datastream relative date\n
    eg '-3M'\n
  endDate -- either a Date or a string with a datastream relative date \n
    eg '-0D'\n
  frequency -- one of the standard Datastream frequencies - D, W, M, Q, or Y
  format -- can be either  \"ByInstrument\" or \"ByDatatype\".

  Returns either a single xts or a list of xts a data.frame with
  the requested data.  If \"ByInstrument\" then the data is returned as
  one or more (ie a list) wide xts with one column per instrument.
  If \"ByDatatype\" then the data is returned as one or more (ie a list)
  of wide xts with one column per Datatype.  This format is more compatible
  with the quantmod package.

  Examples:\n

  mydsws$timeSeriesListRequest(instrument = \"LFTSE100\",\n
    datatype = \"P\", startDate = \"-30D\",\n
    endDate = \"-0D\", frequency = \"D\")

  mydsws$timeSeriesListRequest(instrument = \"LFTSE100\", \n
    expression = \"PCH#(XXXX,3M)\", \n
    startDate = \"-30D\",\n
    endDate = \"-0D\", \n
    frequency = \"D\")

  mydsws$timeSeriesListRequest(instrument = \"LFTSE100\", \n
    datatype = (\"P\",\"UP\"), startDate = \"-30D\",\n
    endDate = \"-0D\", \n
    frequency = \"D\", format = \"ByDatatype\")

  "

  # First return a list of mnemonics

  symbolList <<- .self$.basicRequest(instrument = instrument,
                                     datatype = "MNEM",
                                     expression = "",
                                     isList = TRUE,
                                     startDate = "",
                                     endDate = endDate,
                                     frequency = frequency,
                                     kind = 0,
                                     format = "SnapshotList")


  return(.self$.basicRequest(instrument = symbolList[,1],
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
dsws$methods(.basicRequest = function(instrument,
                                      datatype = "",
                                      expression = "",
                                      isList = FALSE,
                                      startDate,
                                      endDate,
                                      frequency = "D",
                                      kind = 0,
                                      format = "ByInstrument"){

  "
   Internal method.
   This is not intended to be called directly.
   Return a request from Datastream dsws.
   Should request either a datatype or an expression not both.
   If a datatype is provided then anything
   in Expression will be ignored.
   Datatype can be a vector with length > 1, but expression should
   only be of length 1.
   This method will chunk the requests to dsws if necessary.
  "

  if(length(expression) > 1) expression <- expression[1]
  .self$setErrorlist(list())


  # Setting a limit on the number of datatypes means that we will always split instrument up into chunks
  # simplifying the chunking and stitching process.
  if(length(datatype) >= .self$chunkLimit) {
    stop(paste0("The number of datatypes request must be less than the limit of ", .self$chunkLimit))
  }



  #TODO calc num instrument and num Datatype

  if(format == "Snapshot"){
    # Set the holder for the results here
    # Process the response into a dataframe, one row per instrument, with a column for each datatype
    .self$myValues <- data.frame(matrix(NA, nrow = length(instrument), ncol = length(datatype) + 1))
  } else if(format == "ByInstrument"){
    xtsValues <- NULL
  }

  # Holder for the type (ie Date, string) for each of the datatypes
  .self$myTypes <- rep(NA, length(datatype))

  # if we are using expressions then length(datatype) will be 1L and so will not affect the test
  if(length(instrument) * length(datatype) < .self$chunkLimit) {
    # Chunking not required so just pass through the request
    return(.self$.basicRequestChunk(instrument = instrument,
                                    datatype = datatype,
                                    expression = expression,
                                    isList = isList,
                                    startDate = startDate,
                                    endDate = endDate,
                                    frequency = frequency,
                                    kind = kind,
                                    format = format,
                                    chunkNumber = 1,
                                    isChunked = FALSE))

  }

  # Chunking required which will to split instrument into chunks

  maxRequest <- .self$chunkLimit
  numCodes <- length(instrument)
  maxRequest <- floor(.self$chunkLimit / length(datatype))
  numChunks <- ceiling(numCodes / maxRequest )


  for(i in 1:numChunks){
    # get the the list of instruments for each request
    startIndex <- ((i - 1) * maxRequest) + 1
    endIndex <- ifelse((i * maxRequest) < numCodes, (i * maxRequest), numCodes )
    chunkInstrument <- instrument[startIndex:endIndex]
    resRows <- seq(from = startIndex, to = endIndex)

    # make a request for the chunk of instruments
    ret <- .self$.basicRequestChunk(instrument = chunkInstrument,
                                    datatype = datatype,
                                    expression = expression,
                                    isList = isList,
                                    startDate = startDate,
                                    endDate = endDate,
                                    frequency = frequency,
                                    kind = kind,
                                    format = format,
                                    isChunked = TRUE,
                                    chunkNumber = i,
                                    chunkItems = resRows)

    # How we join the results together depends of the nature of the format
    if(format[1] == "ByInstrument"){
      # If the format is by instrument then we have a wide xts, one for each datatype.
      # each of these individual xts will need to be merged with the master

      if(is.null(ret)) {
        .self$setErrorlist(c(.self$getErrorlist(),
                             paste0("Chunk number ", i, " returned a null response")))
        next
      }

      if(length(datatype) == 1){
        # If we have only one datatype then merging is simple
        if(is.null(xtsValues)){
          xtsValues <- ret
        } else {
          xtsValues <- cbind(xtsValues, ret)
        }
      } else {
        # If multiple datatypes then the xts for each datatype has to be merged individually
        for(i in 1: length(datatype)){
          if(is.null(xtsValues[[i]])){
            xtsValues[[i]] <- ret[[i]]
          } else {
            xtsValues[[i]] <- cbind(xtsValues[[i]], ret[[i]])
          }
        }
      }

    } else if(format == "ByDatatype") {
      # ByDatatype might be a simple implementation unless we have too many datatypes.
      # should simply set a limit on the number of datatypes of less than chunk limit.
      stop("chunking of ByDatatype not implemented")
    } else if(format == "Snapshot" | format == "SnapshotList"){
      # Nothing to do :-)
    } else (
      stop("Unknown format type")
    )
  }


  #Finished the chunking loop, so need to return according to the request type

  if(format[1] == "ByInstrument" | format == "ByDatatype"){
    return(xtsValues)
  } else if(format == "Snapshot"){
    return(.self$myValues)
  }

  # should not get here
  return(NULL)


})


#-----------------------------------------------------------------------------
#' @importFrom xts xts
dsws$methods(.basicRequestChunk = function(instrument,
                                           datatype = "",
                                           expression = "",
                                           isList = FALSE,
                                           startDate,
                                           endDate,
                                           frequency = "D",
                                           kind = 0,
                                           format = "ByInstrument",
                                           isChunked = FALSE,
                                           chunkNumber = 0,
                                           chunkItems = NULL){

  "
   Return a timeSeriesRequest from Datastream dsws.  Should request
   either a datatype or an expression not both.  If a datatype is provided
   then anything in Expression will be ignored.
        isChunked - Boolean about whether the request is
                    part of a chunked request
  "

  if(isChunked && format == "SnapshotList") {
    stop("SnapshotList format cannot be chunked.")
  }

  myReq <- .self$.buildRequestList(frequency = frequency,
                                   instrument = instrument,
                                   datatype = datatype,
                                   expression = expression,
                                   isList = isList,
                                   startDate = startDate,
                                   endDate = endDate,
                                   kind = kind,
                                   token = .self$.getToken())

  .self$requestList <- myReq$requestList
  myNumDatatype <- myReq$numDatatype
  myNumInstrument <- myReq$numInstrument

  # Make the request to the server
  ret <- .self$.makeRequest()

  if(.self$logging >=3 ){
    startTime <- Sys.time()
  }

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
    return(xts::xts(NULL))
  }

  myxtsData <- list()

  if(format[1] == "ByInstrument"){
    # If the format is byInstrument, then we are going to create a list of wide xts, one for each datatype

    #    if(isDatatype){
    # We have sent the request as multiple instruments and multiple datatypes so
    # response has a single item in DataTypeValues
    for(iDatatype in 1:myNumDatatype){

      # Create a dataframe to hold the results
      .self$myValues <- data.frame(matrix(NA, nrow = length(myDates), ncol = myNumInstrument))

      # Place the returned data into columns of the dataframe and name the column
      for(iInstrument in 1:myNumInstrument){
        .self$.parseBranch(iInstrument,
                           iDatatype,
                           formatType = "ByInstrument")
      }

      # Turn it into a xts and if more than one datatype was requested put it into a list
      # We could in future save the xts into an environment as well  - a la Quantmod package
      if(myNumDatatype == 1){
        myxtsData <- xts::xts(.self$myValues, order.by = myDates)
      } else {
        myxtsData[[iDatatype]] <- xts::xts(.self$myValues, order.by = myDates)
      }
    }
    if(.self$logging >=3 ){
      message(paste0("Processing from JSON to R took ", Sys.time() - startTime))
    }
    return(myxtsData)

  } else if(format == "ByDatatype"){
    # If the format is byDatatype, then we are going to create a list of wide xts, one for each instrument
    # this is closer to the getSymbols function of the quantmod package and so might be a springboard
    # for extending to that package
    for(iInstrument in 1:myNumInstrument){

      # Create a dataframe to hold the results
      .self$myValues <- data.frame(matrix(NA, nrow = length(myDates), ncol = myNumDatatype))

      # Place the returned data into columns of the dataframe and name the column
      for(iDatatype in 1:myNumDatatype){
        .self$.parseBranch(iInstrument,
                           iDatatype,
                           formatType = "ByInstrument")
      }

      # Turn it into a xts and if more than one datatype was requested put it into a list
      if(myNumInstrument == 1){
        myxtsData <- xts::xts(.self$myValues, order.by = myDates)
      } else {
        myxtsData[[iInstrument]] <- xts::xts(.self$myValues, order.by = myDates)
      }
    }
    if(.self$logging >=3 ){
      message(paste0("Processing from JSON to R took ", Sys.time() - startTime))
    }
    return(myxtsData)

  } else if(format == "Snapshot" | format == "SnapshotList") {

    if(format == "SnapshotList") {
      # If a list request then take the number of instruments from the response
      .self$numInstrument <- length(.self$dataResponse$DataResponse$DataTypeValues[[1]]$SymbolValues)
      .self$myValues <- data.frame(matrix(NA, nrow = .self$numInstrument, ncol = myNumDatatype + 1))
    }

    # Process the column for the instruments
    colnames(.self$myValues)[1] <- "Instrument"

    if(isChunked){
      .self$myValues[chunkItems, 1] <-
        sapply(.self$dataResponse$DataResponse$DataTypeValues[[1]]$SymbolValues,
               FUN = .getSymbol)
    } else {
      .self$myValues[, 1] <-
        sapply(.self$dataResponse$DataResponse$DataTypeValues[[1]]$SymbolValues,
               FUN = .getSymbol)
    }



    # Process the columns of data
    for(iDatatype in 1:myNumDatatype){

      # Put a title on the column
      colnames(.self$myValues)[iDatatype + 1] <-
        make.names(.self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$DataType)


      # On the first run get the type of each datatype and store in an array.  We
      # pick the typical (median) type of the column
      if(chunkNumber == 1) {
        myType <- sapply(.self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues,
                         FUN = .getType,
                         simplify = TRUE)
        .self$myTypes[iDatatype] <- round(median(as.numeric(myType)))

        # On the first loop, we need to check what the type of data is, and if a Date
        # then we need to pre-format the column of the data.frame as a Date

        if(.self$myTypes[iDatatype] == 4) {
          .self$myValues[, iDatatype + 1] <- as.Date((NA))
        }
      }


      # Can't use sapply with simplify or unlist directly as they strip any Date attributes.
      dd <- sapply(.self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues,
                   FUN = .getValueTyped,
                   myType = myTypes[iDatatype],
                   simplify = FALSE)

      if(myTypes[iDatatype] == 4) {
        # as we have a date in the column
        if(isChunked){
          .self$myValues[chunkItems, iDatatype + 1] <- suppressWarnings(as.Date(do.call("c",dd)))
        } else {
          .self$myValues[, iDatatype + 1] <- suppressWarnings(as.Date(do.call("c",dd)))
        }
      } else {
        # as we do not have a Date in this column
        if(isChunked){
          .self$myValues[chunkItems, iDatatype + 1] <- unlist(dd)
        } else {
          .self$myValues[, iDatatype + 1] <- unlist(dd)
        }
      }


    }
    if(.self$logging >=3 ){
      message(paste0("Processing from JSON to R took ", Sys.time() - startTime))
    }
    return(.self$myValues)

  } else {
    stop("Output format is not ByDatatype or ByInstrument")
  }
  # should not get here
  return(NULL)
})


#-----------------------------------------------------------------------------
dsws$methods(.parseBranch = function(iInstrument, iDatatype, formatType){

  # we are using eval to avoid copying what might be a big table of in myValues
  myValuesList <- .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Value

  myValuesList[sapply(myValuesList, is.null)] <- NA

  if(!(TRUE %in% grepl("[$$ER:]", myValuesList[[1]]) )){
    .self$myValues[,iInstrument] <- t(data.frame(lapply(myValuesList, FUN = .convertJSONString)))
  }

  # Add column names
  colnames(.self$myValues)[iInstrument] <-
    make.names(.self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Symbol)
  # Replace errors with NA
  .self$myValues[which(myValues[,iDatatype] == "$$ER: 0904,NO DATA AVAILABLE"),iDatatype] <- NA

  return(NULL)

})

#--------------------------------------------------------------------------------------------
dsws$methods(.buildRequestList = function (frequency, instrument, datatype, expression, isList, startDate, endDate, kind, token) {
  "Internal function that builds a request list that can be then parsed
   to JSON and sent to the DSWS server"

  # Check inputs
  if(!frequency %in% c("D", "W", "M", "Q", "Y")){
    stop("frequency must be one of D, W, M, Q or Y")
  }

  # Only use expressions if datatype is blank.  Expression has to be substituted into instrument
  myNumInstrument <- length(instrument)
  instrument<-toupper(instrument)

  if(datatype == "" && expression != ""){
    myNumDatatype <- 1
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
    myNumDatatype <- length(datatype)
    isDatatype <- TRUE
    myDataType <- lapply(datatype, FUN = function(x) return(list(Properties= NULL, Value = x)))
  }

  # Limit of size of requests
  if(myNumInstrument * myNumDatatype > .self$chunkLimit) {
    stop(paste0("Maximum number of dataitems is in excess of ",
                .self$chunkLimit,
                ".  Internal package error with how requests have been chunked"))
  }

  # If we are making a list request then need to set IsList to be TRUE
  if(isList){
    instrumentProperties <- list(list(Key = "IsList", Value = TRUE))
  } else {
    instrumentProperties <- NULL
  }


  # If more than one instrument then we have to pass IsSymbolSet=TRUE to wsds
  if(myNumInstrument > 1){
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

  # Combine all these elements to create the request (in list form),
  # but also return the number of datatypes and instruments being requested for
  # use when processing the response
  return(list(numDatatype = myNumDatatype,
              numInstrument = myNumInstrument,
              requestList = list(DataRequest = list(DataTypes = myDataType,
                                                    Date = myDates,
                                                    Instrument = myInstrument,
                                                    Tag = NULL),
                                 Properties = list(Properties=NULL),
                                 TokenValue = token))
  )
})



