##############################################################################################

context("classConstructor.R : test of timeSeriesRequest function")
source("~/.RProfile")
suppressMessages(.First())

##############################################################################################

test_that("test of simple timeseries request with relative dates", {



dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)

c<-timeSeriesRequest(dwei = dwei,
                  DSCode = c("MKS"),
                  Instrument = "MV",
                  startDate = as.Date("31/12/2014", "%d/%m/%Y"),
                  endDate = as.Date("31/12/2015", "%d/%m/%Y"),
                  sStockList = stn,
                  aTimeSeries = stpr,
                  verbose=TRUE)
expect_is(stpr, "xts")
rm(c, stn, stpr, dwei)
})

##############################################################################################

test_that("test of two stock timeseries request with relative dates", {

dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)

c<-timeSeriesRequest(dwei = dwei,
                     DSCode = c("MKS","RIO"),
                     Instrument = "MV",
                     startDate = as.Date("31/12/2014", "%d/%m/%Y"),
                     endDate = as.Date("31/12/2015", "%d/%m/%Y"),
                     sStockList = stn,
                     aTimeSeries = stpr,
                     verbose=TRUE)
expect_is(stpr, "xts")
rm(c, stn, stpr, dwei)
})


##############################################################################################

test_that("test of simple timeseries request with absolute dates", {


  dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
Instrument <- c("PCH#(USCONPRCF,12M)")

startDate <- as.Date("31/12/1960", "%d/%m/%Y")
endDate <- as.Date('24/01/2012', '%d/%m/%Y')

a <- timeSeriesRequest(dwei = dwei, DSCode = Instrument,
                       startDate = startDate, frequency = "M", sStockList = stn, aTimeSeries = stpr)
})

