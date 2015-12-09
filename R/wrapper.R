#' @include common.R
#' @include classConstructor.R
NULL

# This is a set of functions that will provide backwards compatability with
# the Datastream2R package


#  Functions are:
#     getDatastream - returns a connection to the DWE server and generate S3 objects
#     listRequest - Function that returns a the value of Expression for the instrument list in DSCode
#                In Datastream AFo this is Static request on a List eg LS&PCOMP
#     timeSeriesListRequest - Function that returns a set of timeseries for an instrument list eg LS&PCOMP
#     timeSeriesRequest - Function that returns a series of timeseries for a list of instruments
#
#




##############################################################################################
#'
#'
#' @title Initialise connection with Datastream DSWS server
#' @details Initialise connection with Datastream DSWS server
#' @description \code{getDataStream} initialises the connection with the Datastream DWE server
#' @param dweURLwsdl The URL of the server
#' @param User The username for Datastream.  If not provided it will use the username from the windows registry
#' @param Pass The password for Datastream.  Also sourced from Registry
#' @return a DWE connection object
#'
#' @export
#'
getDataStream <- function(dweURLwsdl = "",
                          User=as.character("USERNAME"),
                          Pass=as.character("PASSWORD")
){

  return(dsws$new())
  }


###############################################################################################
#' @title make a timeseries request
#'
#' @details \code{timeSeriesRequest} Function that obtains a timeseries from Datastream
#' parameters are
#'
#' @param    dwei - A Datastream Client Interface object created with getDataStream
#' @param    DSCodes - one or more codes to return, eg "MKS" or c("MKS","SAB")
#' @param    Instrument - the instrument or expression to return eg PCH#(XXXX,1M)
#' @param    startDate - the start date of the timeseries
#' @param    endDate - the end date of the timeseries
#' @param    frequency - the frequency of the request
#' @param    sStockList - variable that is returned with list of of the stocks
#' @param    aTimeSeries - variable that is returned with the set of timeseries.  This is a list that is not
#' guaranteed to be in the same order as sStockList
#' @param    myType - the type of the return values eg numeric (default), Date or Character
#' @param    verbose - whether to give messages during the request
#'

#' @return    whether the request has been successful
#'    in sStockList: a list a two element vector of the displayname and symbol for each timeseries
#'    in aTimeseries: a list of class xts with the requested timeseries information
#'
#'
#' @export
timeSeriesRequest <- function (dwei=getDataStream(),
                               DSCodes="",
                               Instrument="",
                               startDate=Sys.Date(),
                               endDate=Sys.Date(),
                               frequency="D",
                               sStockList,
                               aTimeSeries,
                               myType = "numeric",
                               verbose=FALSE) {

if(grepl(pattern="XXXX", x=Instrument, fixed=TRUE) == FALSE){
  # Instrument is not an expression if it does not contain 'XXXX
  myDataType <- Instrument
  myExpression <- ""
} else {
  myDataType <- ""
  myExpression <- Instrument
}


myxts <- dwei$timeSeriesRequest(instrument = DSCodes,
                                datatype = myDataType,
                                expression = myExpression,
                                startDate = startDate,
                                endDate = endDate,
                                frequency = frequency,
                                format = "ByInstrument")

eval.parent(substitute(sStockList <- "To be implemented"))
eval.parent(suppressWarnings(substitute(aTimeSeries <- myxts)))

return("TO BE IMPLEMENTED instrument code map")
}
