##############################################################################################

context("classConstructor.R : test of timeSeriesRequest function")

##############################################################################################

test_that("test of simple timeseries request with relative dates", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()



  dwei <- getDataStream(User=Sys.getenv("DatastreamUsername"), Pass=Sys.getenv("DatastreamPassword"))
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
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  dwei <- getDataStream(User=Sys.getenv("DatastreamUsername"), Pass=Sys.getenv("DatastreamPassword"))
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
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  dwei <- getDataStream(User=Sys.getenv("DatastreamUsername"), Pass=Sys.getenv("DatastreamPassword"))
  Instrument <- c("PCH#(USCONPRCF,12M)")

  startDate <- as.Date("31/12/1960", "%d/%m/%Y")
  endDate <- as.Date('24/01/2012', '%d/%m/%Y')

  a <- timeSeriesRequest(dwei = dwei,
                         DSCode = Instrument,
                         startDate = startDate,
                         frequency = "M",
                         sStockList = stn,
                         aTimeSeries = stpr)
})

