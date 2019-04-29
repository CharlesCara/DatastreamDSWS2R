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
#' @import methods
#' @export dsws
#' @exportClass dsws
dsws <- setRefClass(Class="dsws",
                    fields = list(tokenList="ANY",
                                  serverURL = "character",
                                  username = "character",
                                  password = "character",
                                  initialised = "logical",
                                  errorlist = "ANY",
                                  requestList = "ANY",
                                  jsonResponseSaveFile = "ANY",
                                  jsonResponseLoadFile = "ANY",
                                  dataResponse = "ANY",
                                  symbolList = "ANY",
                                  myValues = "ANY",
                                  myTypes = "ANY",
                                  logging = "numeric",
                                  numDatatype = "numeric",
                                  numInstrument = "numeric",
                                  chunkLimit = "numeric",
                                  requestStringLimit = "integer",
                                  logFileFolder = "character"))

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
  provided, then they are sourced from system enviroment variables (ie Sys.getenv)
      'DatastreamUsername' and 'DatastreamPassword'
  or failing that from
      options()$Datastream.Username and
      options()$Datastream.Password

    This allows the password to be stored in .Renviron or .RProfile rather
  than in the source code.

  There is a difference in the Refinitiv's documentation about the chunk limit and different accounts have
  different limits.  Some users are limited to 50 items while others are limited to 2000L.  The chunk limit
  can be controlled by setting the chunkLimit parameter of the dsws object.  If options()$Datastream.ChunkLimit is
  set then the value is taken from there.

  "

  .self$initialised <<- FALSE
  .self$errorlist <<- NULL
  if(is.null(options()$Datastream.ChunkLimit)){
      .self$chunkLimit <<- 2000L   # Max number of items that can be in a single request.  Set by Datastream
  } else {
    .self$chunkLimit <<- as.integer(options()$Datastream.ChunkLimit)
      }
  .self$requestStringLimit <<- 2000L # Max character length of an http request.
  .self$logging <<- 0L
  .self$logFileFolder <<- Sys.getenv("R_USER")
  .self$jsonResponseLoadFile <<- NULL  # By default is to hit the server
  .self$jsonResponseSaveFile <<- NULL # Default is not to save JSON response

  if(dsws.serverURL == ""){
    # 07/4/2016 - due to issue with Datastream's load balancers, using a different URL.  This will
    # be changed back when the issue is resolved.
    .self$serverURL <<- "http://product.datastream.com/DSWSClient/V1/DSService.svc/rest/"
  } else {
    .self$serverURL <<- dsws.serverURL
  }

  if(username != ""){
    .self$username <<- username
  } else if(Sys.getenv("DatastreamUsername") != ""){
    .self$username <<- Sys.getenv("DatastreamUsername")
  } else if(!is.null(options()$Datastream.Username)){
    .self$username <<- options()$Datastream.Username
  } else {
    stop("Either username must be specified or it must be set via options(\"Datastream.Username\", \"Myusername\"")
  }


  if(password != ""){
    .self$password <<- password
  } else if(Sys.getenv("DatastreamPassword") != ""){
    .self$password <<- Sys.getenv("DatastreamPassword")
  } else if(!is.null(options()$Datastream.Password)){
    .self$password <<- options()$Datastream.Password
  } else {
    stop("Either username must be specified or it must be set via options(\"Datastream.Password\", \"Mypassword\"")
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
  if(is.null(ts$TokenValue) || is.null(ts$TokenExpiry) || Sys.time() > ts$TokenExpiry ){
    # Either we do not already have a token, or it has expired, so we need to request one

    myTokenURL <- paste0(.self$serverURL, "Token",
                         "?username=", .self$username ,
                         "&password=", .self$password )

    # We are going to handle timeouts by
    # waiting incrementally up to 16 sec and repeating the request
    maxLoop <- 4L
    waitTimeBase <- 2
    nLoop <- 0
    .self$errorlist <- NULL

    handleCurlError <- function(e){
      message("Error requesting token...")
      message(as.character(e))
      .self$errorlist <- c(.self$errorlist, list(list(request = "Token request", error = e)))
      return(NULL)}

    repeat{
      myTokenResponse <- tryCatch(RCurl::getURL(url = myTokenURL),
                                  error = function(e) e)

      # Success so leave loop
      if(!is.null(myTokenResponse) &&
         !("error" %in% class(myTokenResponse)) &&
         !("list" %in% class(myTokenResponse))) break


      # write out error message and store it
      message("Error requesting token...")
      message(as.character(myTokenResponse))
      .self$errorlist <- c(.self$errorlist, list(list(request = "Token request", error = myTokenResponse)))


      # Only retry timeouts
      if(!("GenericCurlError" %in% class(myTokenResponse) &&
           str_detect(myTokenResponse$message, "Timed out"))) break


      # Too many tries
      if(nLoop >= maxLoop) break

      message("...retrying")
      Sys.sleep(waitTimeBase ^ nLoop)
      nLoop <- nLoop + 1
    }


    if(is.null(myTokenResponse)){
      stop("Could not request access Token - response from server was NULL")
    }

    myTokenList <- rjson::fromJSON(myTokenResponse)

    #Error check response
    if(!is.null(myTokenList$Code)){
      .self$tokenList <- list(TokenValue = NULL,
                              TokenExpiry = NULL)

      stop(paste0("Error requesting access Token.  Message was:\n",
                  myTokenList$Code, "\n",
                  myTokenList$Message))
    } else {
      .self$tokenList <- list(TokenValue = myTokenList$TokenValue,
                              TokenExpiry = .convert_JSON_Datetime(myTokenList$TokenExpiry))
    }
  }


  return(invisible(.self$tokenList$TokenValue))
})


#-------validToken----------------------------------------------------------------------

dsws$methods(.tokenExpired = function(thisToken = NULL, myTime = Sys.time()){
  "Checks whether the given or saved token has not expired.
   Returns true if it has, false otherwise"

  if(is.null(thisToken)){
    thisToken <- .self$tokenList
  }
  thisTokenExpiry <- thisToken$TokenExpiry

  if(is.null(thisTokenExpiry)) return(TRUE)

  # We want the token to have at least one hour before expiry
  return( (thisTokenExpiry - myTime) < as.difftime(1, units = "hours") )

})


#-----------------------------------------------------------------------------
#' @importFrom rjson toJSON fromJSON
#' @importFrom stringr str_detect
dsws$methods(.makeRequest = function(bundle = FALSE){
  "Internal function: make a request from the DSWS server.
  The request  (in a R list form) is taken from .self$requestList,
  parsed into JSON and sent to the DSWS server.  The JSON response is
  parsed and saved in .self$dataResponse"

  # This option is for testing purposes.  The response is loaded
  # from a specified JSON file, rather than the DSWS server

  if(!is.null(.self$jsonResponseLoadFile)){
    if(file.exists(.self$jsonResponseLoadFile)) {
      .self$dataResponse <- rjson::fromJSON(file = .self$jsonResponseLoadFile)
      return(TRUE)
    } else {
      stop("File specified by dsws$jsonResponseLoadFile does not exist")
    }
  }


  if(bundle){
    myDataURL <- paste0(.self$serverURL , "GetDataBundle")
  }else{
    myDataURL <- paste0(.self$serverURL , "GetData")
  }

  if(.self$.tokenExpired()){
    .self$.getToken()
  }

  myRequestJSON <- rjson::toJSON(.self$requestList)

  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")


  if(.self$logging >=5 ){
    message("JSON request to DSWS server response is:\n")
    message(myRequestJSON)
    message("--------------------------------------------------")
  }

  # We are going to handle timeouts by
  # waiting incrementally up to 16 sec and repeating the request
  maxLoop <- 4L
  waitTimeBase <- 2
  nLoop <- 0
  .self$errorlist <- NULL
  repeat{
    myDataResponse <- tryCatch({
      RCurl::postForm(myDataURL,
                      .opts=list(httpheader=httpheader
                                 ,postfields=myRequestJSON))
    },
    error = function(e) e)

    # Success so leave loop
    if(!is.null(myDataResponse) &&
       !("error" %in% class(myDataResponse)) &&
       !("list" %in% class(myDataResponse))) break


    # write out error message and store it
    message("Error requesting json...")
    message(as.character(myDataResponse))
    .self$errorlist <- c(.self$errorlist, list(list(request = myRequestJSON, error = myDataResponse)))


    # Only retry timeouts
    if(!("GenericCurlError" %in% class(myDataResponse) &&
         str_detect(myDataResponse$message, "Timed out"))) {
      message("GenericCurlError:")
      message(myDataResponse$message)
      message(class(myDataResponse))
      break
    }
    # Too many tries
    if(nLoop >= maxLoop) break

    message("...retrying")
    Sys.sleep(waitTimeBase ^ nLoop)
    nLoop <- nLoop + 1
  }

  if(.self$logging >=5 ){
    message("DSWS server response is:\n")
    message(myDataResponse)
    message("--------------------------------------------------")
  }


  if(!is.null(.self$jsonResponseSaveFile)){
    if(!is.null(myDataResponse)){
      writeChar(object = myDataResponse, con = .self$jsonResponseSaveFile)
    }
  }


  if(is.null(myDataResponse) || "list" %in% class(myDataResponse)){
    .self$dataResponse <-  NULL
  } else {
    .self$dataResponse <- rjson::fromJSON(json_str = myDataResponse)
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
                             startDate = requestDate,
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

  myData <- .self$.basicRequest(instrument = instrument,
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
  # expression has to be atomic and not an array
  if(length(expression) > 1) expression <- expression[1]
  .self$setErrorlist(list())

  # We have to have at least one instrument
  numCodes <- length(instrument)
  if(numCodes == 0){
    stop("instruments is empty and has length zero")
  }

  # Setting a limit on the number of datatypes means that we will always split instrument up into chunks
  # simplifying the chunking and stitching process.
  if(length(datatype) >= .self$chunkLimit) {
    stop(paste0("The number of datatypes request must be less than the limit of ", .self$chunkLimit))
  }

  # names of can upset the format of the json request
  names(instrument) <- NULL
  names(datatype) <- NULL
  names(expression) <- NULL

  if(format == "Snapshot"){
    # Set the holder for the results here
    # Process the response into a dataframe, one row per instrument, with a column for each datatype
    .self$myValues <- data.frame(matrix(NA, nrow = length(instrument), ncol = length(datatype) + 1))
  } else if(format == "ByInstrument"){
    xtsValues <- NULL
  }


  # Holder for the type (ie Date, string) for each of the datatypes
  .self$myTypes <- rep(NA, length(datatype))


  doChunk <- FALSE
  if(datatype[1] != ""){
    # If we are not using a expression, we will just apply the rule that
    # number of instruments * number of datatypes has to be less tha the chunk limit
    doChunk <- (length(instrument) * length(datatype) >= .self$chunkLimit)
  } else {
    # There appears to be a maximum character limit for a request (or response)
    # We will need to chunk the request if we are using an expression and when we expand the expression
    # it is over this limit.
    expandedInstrument <- paste0(.self$.expandExpression(instrument, expression), collapse=",")
    if((nchar(expandedInstrument) >= .self$requestStringLimit) |
       (length(instrument) * length(datatype) >= .self$chunkLimit)){
      doChunk <- TRUE
    }
  }

  # if we are using expressions then length(datatype) will be 1L and so will not affect the test
  if(!doChunk) {
    # Chunking not required so just pass through the request
    if(format == "Snapshot" | format == "SnapshotList"){
      return(.self$.basicRequestSnapshotChunk(instrument = instrument,
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
    } else  {
      return(.self$.basicRequestTSChunk(instrument = instrument,
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
  }

  # Chunking required which will to split instrument into chunks
  # Work out the number of chunks and the size of each request

  if(datatype[1] != ""){
    numInstrChunk <- floor(.self$chunkLimit / length(datatype))
    numChunks <- ceiling(numCodes / numInstrChunk )

  } else {
    # If we are using expressions then we have to choose our number of chunks as the larger
    # of the number defined by the limit on the number of instruments and the number
    # defined by the limit of the request string length

    numChunksInst <- ceiling(numCodes / .self$chunkLimit )
    expandedInstrument <- paste0(.self$.expandExpression(instrument, expression), collapse=",")
    numChunksString <-  ceiling(nchar(expandedInstrument) / .self$requestStringLimit)

    numInstrChunk <- floor(numCodes / max(numChunksInst, numChunksString))
    numChunks <- ceiling(numCodes / numInstrChunk )


  }



  for(i in 1:numChunks){
    # get the the list of instruments for each request
    startIndex <- ((i - 1) * numInstrChunk) + 1
    endIndex <- ifelse((i * numInstrChunk) < numCodes, (i * numInstrChunk), numCodes )
    chunkInstrument <- instrument[startIndex:endIndex]
    resRows <- seq(from = startIndex, to = endIndex)

    # make a request for the chunk of instruments
    if(format == "Snapshot" | format == "SnapshotList"){
      ret <- .self$.basicRequestSnapshotChunk(instrument = chunkInstrument,
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
    } else  {
      ret <- .self$.basicRequestTSChunk(instrument = chunkInstrument,
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
    }


    # How we join the results together depends of the nature of the format
    if(format[1] == "ByInstrument"){
      # If the format is by instrument then we have a wide xts, one for each datatype.
      # each of these individual xts will need to be merged with the master

      if(is.null(ret)) {
        .self$setErrorlist(c(.self$getErrorlist(),
                             list(message = paste0("Chunk number ", i, " returned a null response"))))
        next
      }

      if(length(datatype) == 1){
        # If we have only one datatype then merging is simple
        if(is.null(xtsValues)){
          xtsValues <- ret
        } else {
          xtsValues <- cbindRobust(xtsValues, ret)
        }
      } else {
        # If multiple datatypes then the xts for each datatype has to be merged individually
        for(i in 1: length(datatype)){
          if(is.null(xtsValues[[i]])){
            xtsValues[[i]] <- ret[[i]]
          } else {
            xtsValues[[i]] <- cbindRobust(xtsValues[[i]], ret[[i]])
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
dsws$methods(.basicRequestTSChunk = function(instrument,
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

  if(.self$logging >=3 ){
    message("Making request for:")
    message(paste0("Number of instruments: ", length(instrument)))
    message(paste0("Number of datatypes:   ", length(datatype)))
    message(paste0("Number of expressions: ", length(expression)))
  }
  myReq <- .self$.buildRequestListBundle(frequency = frequency,
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
  ret <- .self$.makeRequest(bundle = TRUE)

  if(!ret){
    # There has been an error.  Return NULL.  Error is stored in .self$errorlist
    return(NULL)
  }

  # Now to parse the timeseries data in myData into an xts
  # If we have more than one dimension then it is returned as a list of wide xts

  # Get the dates - these are provided separately
  myDates <- .self$.parseDatesBundle()

  if(.self$logging >=3 ){
    message("Response contained:")
    message(paste0("Number of dates in response: ", length(myDates)))
    message(paste0("Number of DataTypeValues returned: ",
                   length(.self$dataResponse$DataResponses[[1]]$DataTypeValues)))
    message(paste0("Number of SymbolValues returned for first DataTypeValue: ",
                   length(.self$dataResponse$DataResponses[[1]]$DataTypeValues[[1]]$SymbolValues)))

  }

  if(length(myDates) == 0 ){
    # If the length of the Dates object is 0 then no data has been returned

    if(format[1] == "ByInstrument"){
      # return an xts with the same number of columns as instrument and no rows
      if(myNumDatatype == 1){
        myxtsData <- xts::xts(matrix(NA, nrow = 1, ncol = myNumInstrument), order.by = as.Date("2017-01-01"))["20170131"]
        colnames(myxtsData) <- instrument
      } else {
        myxtsData <- list()
        myxts <- xts::xts(matrix(NA, nrow = 1, ncol = myNumInstrument), order.by = as.Date("2017-01-01"))["20170131"]
        colnames(myxts) <- instrument
        for(i in 1: myNumDatatype){
          myxtsData[[i]] <- myxts
        }
      }
      return(myxtsData)

    } else if(format == "ByDatatype"){
      # return an xts with the same number of columns as datatype and a single NA row
      if(myNumInstrument == 1){
        myxtsData <- xts::xts(matrix(NA, nrow = 1L, ncol = myNumDatatype), as.Date("2017-01-01"))["20170131"]
        colnames(myxtsData) <- instrument
      } else {
        myxtsData <- list()
        myxts <- xts::xts(matrix(NA, nrow = 1L, ncol = myNumDatatype), as.Date("2017-01-01"))["20170131"]
        colnames(myxts) <- instrument
        for(i in 1: myNumInstrument){
          myxtsData[[i]] <- myxts
        }
      }
      return(myxtsData)

    }
    return(xts::xts(NULL))
  }

  if(format[1] == "ByInstrument"){
    return(.self$.processTimeSeriesByInstrument(myDates = myDates,
                                                myNumDatatype = myNumDatatype,
                                                myNumInstrument = myNumInstrument))

  } else if(format == "ByDatatype"){
    return(.self$.processTimeSeriesByDatatype(myDates = myDates,
                                              myNumDatatype = myNumDatatype,
                                              myNumInstrument = myNumInstrument))

  }

  stop("Output format is one of allowed types: ByDatatype or ByInstrument")
  return(NULL)
})



#-----------------------------------------------------------------------------
#' @importFrom xts xts
dsws$methods(.processTimeSeriesByInstrument = function(myDates, myNumDatatype, myNumInstrument){
  # If the format is byInstrument, then we are going to create a list of wide xts, one for each datatype
  if(.self$logging >=3 ){
    message("Processing byInstrument")
    message("myNumDatatype is ", myNumDatatype)
  }

  myxtsData <- list()
  # We have sent the request as multiple instruments and multiple datatypes so
  # response has a single item in DataTypeValues
  for(iDatatype in 1:myNumDatatype){

    # Create a dataframe to hold the results - first column contains the dates
    .self$myValues <- data.frame(matrix(NA, nrow = length(myDates), ncol = myNumInstrument + 1))
    .self$myValues[,1] <- myDates

    if(.self$logging >=3 ){
      message("iDatatype is ", iDatatype)
      message("myNumInstrument is ", myNumInstrument)
    }
    # Place the returned data into columns of the dataframe and name the column
    for(iInstrument in 1:myNumInstrument){
      .self$.parseBundleBranch(iDRs = iInstrument,
                               iDTV = iDatatype,
                               iSV = 1,
                               iCol = iInstrument + 1,
                               formatType = "ByInstrument")
    }

    # Turn it into a xts and if more than one datatype was requested put it into a list
    # We could in future save the xts into an environment as well  - a la Quantmod package
    if(myNumDatatype == 1){
      myxtsData <- xts::xts(.self$myValues[ ,2:(myNumInstrument + 1)], order.by = myDates)
      colnames(myxtsData) <- colnames(.self$myValues)[2:(myNumInstrument + 1)]
    } else {
      myxtsData[[iDatatype]] <- xts::xts(.self$myValues[ ,2:(myNumInstrument + 1)], order.by = myDates)
      colnames(myxtsData[[iDatatype]]) <- colnames(.self$myValues)[2:(myNumInstrument + 1)]
    }
  }

  return(myxtsData)
})

#-----------------------------------------------------------------------------
#' @importFrom xts xts
dsws$methods(.processTimeSeriesByDatatype = function(myDates, myNumDatatype, myNumInstrument){
  # If the format is byDatatype, then we are going to create a list of wide xts, one for each instrument
  # this is closer to the getSymbols function of the quantmod package and so might be a springboard
  # for extending to that package

  if(.self$logging >=3 ){
    message("Processing byInstrument")
    message("myNumDatatype is ", myNumDatatype)
  }

  myxtsData <- list()
  for(iInstrument in 1:myNumInstrument){

    # Create a dataframe to hold the results - first column contains the dates
    .self$myValues <- data.frame(matrix(NA, nrow = length(myDates), ncol = myNumDatatype + 1))
    .self$myValues[,1] <- myDates

    # Place the returned data into columns of the dataframe and name the column
    for(iDatatype in 1:myNumDatatype){
      .self$.parseBundleBranch(iDRs = iInstrument,
                               iDTV = iDatatype,
                               iSV = 1,
                               iCol = iDatatype + 1,
                               formatType = "ByInstrument")
    }

    # Turn it into a xts and if more than one datatype was requested put it into a list
    if(myNumInstrument == 1){
      myxtsData <- xts::xts(.self$myValues[ ,2:(myNumDatatype + 1)], order.by = myDates)
      colnames(myxtsData) <- colnames(.self$myValues)[2:(myNumDatatype + 1)]
    } else {
      myxtsData[[iInstrument]] <- xts::xts(.self$myValues[ ,2:(myNumDatatype + 1)], order.by = myDates)
      colnames(myxtsData[[iInstrument]]) <- colnames(.self$myValues)[2:(myNumDatatype + 1)]
    }
  }

  return(myxtsData)

})


#-----------------------------------------------------------------------------
#' @importFrom xts xts
dsws$methods(.basicRequestSnapshotChunk = function(instrument,
                                                   datatype = "",
                                                   expression = "",
                                                   isList = FALSE,
                                                   startDate,
                                                   endDate,
                                                   frequency = "D",
                                                   kind = 0,
                                                   format = "Snapshot",
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
  if(format != "Snapshot" & format != "SnapshotList") {
    stop("Output format is one of allowed types: Snapshot or SnapshotList")
  }
  if(isChunked && format == "SnapshotList") {
    stop("SnapshotList format cannot be chunked.")
  }

  if(.self$logging >=3 ){
    message("Making request for:")
    message(paste0("Number of instruments: ", length(instrument)))
    message(paste0("Number of datatypes:   ", length(datatype)))
    message(paste0("Number of expressions: ", length(expression)))
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
  ret <- .self$.makeRequest(bundle = FALSE)

  if(!ret){
    # There has been an error.  Return NULL.  Error is stored in .self$errorlist
    return(NULL)
  }


  if(.self$logging >=3 ){
    message("Response contained:")
    message(paste0("Number of DataTypeValues returned: ",
                   length(.self$dataResponse$DataResponse$DataTypeValues)))
    message(paste0("Number of SymbolValues returned for first DataTypeValue: ",
                   length(.self$dataResponse$DataResponse$DataTypeValues[[1]]$SymbolValues)))

  }


  return(.self$.processSnapshot(format = format,
                                myNumDatatype = myNumDatatype,
                                isChunked = isChunked,
                                chunkItems = chunkItems,
                                chunkNumber = chunkNumber))
})

#-----------------------------------------------------------------------------
#' @importFrom xts xts
dsws$methods(.processSnapshot = function(format, myNumDatatype, isChunked, chunkItems, chunkNumber){
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
                 FUN = .getValue,
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

  return(.self$myValues)


})


#-----------------------------------------------------------------------------
#' @importFrom stringr coll str_detect
dsws$methods(.parseBranch = function(iInstrument, iDatatype, formatType){

  # we are using eval to avoid copying what might be a big table of in myValues
  myValuesList <- .self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Value

  myValuesList[sapply(myValuesList, is.null)] <- NA

  if(!(TRUE %in% str_detect(string = myValuesList[[1]], pattern = coll("$$ER:")) |
       TRUE %in% str_detect(string = myValuesList[[1]], pattern = coll("MainframeAccessPoint error")))){
    # We only process if we have not got an error message

    .self$myValues[,iInstrument] <- t(data.frame(lapply(myValuesList, FUN = .convertJSONString)))
  }

  # Add column names
  colnames(.self$myValues)[iInstrument] <-
    make.names(.self$dataResponse$DataResponse$DataTypeValues[[iDatatype]]$SymbolValues[[iInstrument]]$Symbol)
  # Replace errors with NA
  .self$myValues[which(myValues[,iDatatype] == "$$ER: 0904,NO DATA AVAILABLE"),iDatatype] <- NA
  .self$myValues[which(myValues[,iDatatype] == "MainframeAccessPoint error.Timed out waiting for a response from mainframe."),iDatatype] <- NA

  return(NULL)

})


#-----------------------------------------------------------------------------
#' @importFrom stringr coll str_detect
dsws$methods(.parseBundleBranch = function(iDRs, iDTV, iSV, iCol,  formatType){
  "This function parses a branch of the getDataBundle response.  It assumes that
  a branch only has data for one instrument in it (ie SymbolValues is of
  length 1"


  # we are using eval to avoid copying what might be a big table of in myValues
  lengthSV <- length(.self$dataResponse$DataResponses[[iDRs]]$DataTypeValues[[iDTV]]$SymbolValues)

  if(is.null(lengthSV) | lengthSV == 0) {
    # No data has been returned
    # so we do no need to put anything into the

    return(NULL)
  }

  myValuesList <- .self$dataResponse$DataResponses[[iDRs]]$DataTypeValues[[iDTV]]$SymbolValues[[iSV]]$Value

  myValuesList[sapply(myValuesList, is.null)] <- NA
  myDates <- .convert_JSON_Date(dataResponse$DataResponses[[iDRs]]$Dates)

  if(!(TRUE %in% str_detect(string = myValuesList[[1]], pattern = coll("$$ER:")) |
       TRUE %in% str_detect(string = myValuesList[[1]], pattern = coll("MainframeAccessPoint error")))){
    # We only process if we have not got an error message
    # match up the rows with the existing tables

    .self$myValues[match( myDates, .self$myValues[,1]),iCol]<-
      t(data.frame(lapply(myValuesList, FUN = .convertJSONString)))
  }

  # Add column names
  colnames(.self$myValues)[iCol] <-
    make.names(.self$dataResponse$DataResponses[[iDRs]]$DataTypeValues[[iDTV]]$SymbolValues[[iSV]]$Symbol)

  # Replace errors with NA
  .self$myValues[which(myValues[,iCol] == "$$ER: 0904,NO DATA AVAILABLE"),iCol] <- NA
  .self$myValues[which(myValues[,iCol] == "MainframeAccessPoint error.Timed out waiting for a response from mainframe."),iCol] <- NA



  return(NULL)

})

#--------------------------------------------------------------------------------------------
dsws$methods(.parseDatesBundle = function(){
  numResponses <- length(.self$dataResponse$DataResponses)

  if(numResponses == 0) return(NULL)


  for(i in 1:numResponses){
    if(i==1){
      myDates <- .convert_JSON_Date(dataResponse$DataResponses[[i]]$Dates)

    } else {
      myDates <- c(myDates, .convert_JSON_Date(dataResponse$DataResponses[[i]]$Dates))
    }
  }

  return(sort(unique(myDates)))
})



#--------------------------------------------------------------------------------------------
dsws$methods(.expandExpression = function (instrument, expression){
  "Internal function the expands an expression with all the instruments.
  Returns a string."
  if(expression == ""){
    myString <- instrument
  } else {
    # If the instrument is NA we need to return invalid code
    myString <- sapply(instrument,
                       FUN=function(x) {if(is.na(x)) {"ABCDEFGH"}
                         else
                         {gsub(pattern="XXXX", replacement=x, x=expression, fixed=TRUE)}},
                       USE.NAMES = FALSE)
  }


  return(myString)
})

#--------------------------------------------------------------------------------------------
dsws$methods(.buildRequestList = function (frequency, instrument, datatype, expression, isList, startDate, endDate, kind, token) {
  "Internal function that builds a request list that can be then parsed
   to JSON and sent to the DSWS server.  If bundle is true then the request is
   built for the getDataBundle request page, else for the getData page"

  # Check inputs
  if(!frequency %in% c("D", "W", "M", "Q", "Y")){
    stop("frequency must be one of D, W, M, Q or Y")
  }

  # Only use expressions if datatype is blank.  Expression has to be substituted into instrument.
  # If bundle is true, then we want to put each expression into an individual entry in DataRequests
  myNumInstrument <- length(instrument)
  instrument<-toupper(instrument)

  if(datatype == "" && expression != ""){
    # We have an expression
    myNumDatatype <- 1
    isDatatype <- FALSE
    if( grepl(pattern="XXXX", x=expression, fixed=TRUE) == FALSE){
      # Expression does not contain XXXX so we cannot do a replace
      stop("Expressions must contain XXXX so that they can be inserted into instrument")
    } else {
      # convert instrument by inserting instruments into the Expression
      instrument <- .self$.expandExpression(instrument, expression)
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

  dsRequest <- list(DataRequest = list(DataTypes = myDataType,
                                       Date = myDates,
                                       Instrument = myInstrument,
                                       Tag = NULL),
                    Properties = list(Properties=NULL),
                    TokenValue = token)



  return(list(numDatatype = myNumDatatype,
              numInstrument = myNumInstrument,
              requestList = dsRequest)
  )
})



#--------------------------------------------------------------------------------------------
dsws$methods(.buildRequestListBundle = function (frequency, instrument, datatype, expression, isList, startDate, endDate, kind, token) {
  "Internal function that builds a request list that can be then parsed
  to JSON and sent to the DSWS server for the getDataBundle request page"

  # Check inputs
  if(!frequency %in% c("D", "W", "M", "Q", "Y")){
    stop("frequency must be one of D, W, M, Q or Y")
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

  # If we are making a list request then need to set IsList to be TRUE
  if(isList){
    instrumentProperties <- list(list(Key = "IsList", Value = TRUE))
  } else {
    instrumentProperties <- NULL
  }


  # Only use expressions if datatype is blank.  Expression has to be substituted into instrument.
  # If bundle is true, then we want to put each expression into an individual entry in DataRequests
  myNumInstrument <- length(instrument)
  instrument <- toupper(instrument)

  if(datatype == "" && expression != ""){
    # We have an expression
    myNumDatatype <- 1L
    if( grepl(pattern="XXXX", x=expression, fixed=TRUE) == FALSE){
      # Expression does not contain XXXX so we cannot do a replace
      stop("Expressions must contain XXXX so that they can be inserted into instrument")
    } else {
      # convert instrument by inserting instruments into the Expression
      instrument <- .self$.expandExpression(instrument, expression)
    }
    myDataType <- list()

  } else {
    myNumDatatype <- length(datatype)
    isDatatype <- TRUE
    myDataType <- lapply(datatype, FUN = function(x) return(list(Properties= NULL, Value = x)))

  }

  myDataRequests <- lapply(instrument,
                           FUN = function(x, instProp, myDTP, myDT) {
                             list(DataTypes = myDTP,
                                  Date = myDT,
                                  Instrument = list(Properties= instProp,
                                                    Value = x),
                                  Tag = NULL)},
                           instProp = instrumentProperties,
                           myDTP = myDataType,
                           myDT = myDates)

  # Limit of size of requests
  if(myNumInstrument * myNumDatatype > .self$chunkLimit) {
    stop(paste0("Maximum number of dataitems is in excess of ",
                .self$chunkLimit,
                ".  Internal package error with how requests have been chunked"))
  }

  # Combine all these elements to create the request (in list form),
  # but also return the number of datatypes and instruments being requested for
  # use when processing the response

  dsRequest <- list(DataRequests = myDataRequests,
                    Properties = list(Properties=NULL),
                    TokenValue = token)

  return(list(numDatatype = myNumDatatype,
              numInstrument = myNumInstrument,
              requestList = dsRequest)
  )
})



