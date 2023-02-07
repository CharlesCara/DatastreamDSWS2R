##############################################################################################

context("classConstructor.R : test of timeSeriesListRequest method")

#suppressPackageStartupMessages(require(xts))
##############################################################################################

test_that("test of timeseries list request for price datatype with relative dates", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()



  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         datatype = "P",
                                         startDate = "-10D",
                                         endDate = "0D",
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(zoo::coredata(myData)[1,2], "numeric")
  expect_gt(ncol(myData), 99)
  expect_equal(nrow(myData), 11)

})

test_that("test of timeseries list request for default datatype with relative dates", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()



  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         datatype = "",
                                         startDate = "-10D",
                                         endDate = "0D",
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(zoo::coredata(myData)[1,2], "numeric")
  expect_gt(ncol(myData), 99)
  expect_equal(nrow(myData), 11)

})


test_that("test of timeseries list request for price datatype with absolute dates", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()



  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         datatype = "P",
                                         startDate = as.Date("2015-09-27"),
                                         endDate = as.Date("2015-09-30"),
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(zoo::coredata(myData)[1,2], "numeric")
  expect_gt(ncol(myData), 99)
  expect_equal(nrow(myData), 4)

})


test_that("test of timeseries list request for expression with relative date", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         expression = "110E(XXXX)",
                                         startDate = as.Date("2015-09-27"),
                                         endDate = as.Date("2015-09-30"),
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(zoo::coredata(myData)[1,2], "numeric")
  expect_gt(ncol(myData), 99)
  expect_equal(nrow(myData), 4)

})



test_that("test of timeseries list request for user created list that has a mixture of indices and expressions", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "L#CROSS",
                                         expression = "",
                                         startDate = as.Date("2015-09-27"),
                                         endDate = as.Date("2015-09-30"),
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(zoo::coredata(myData)[1,2], "numeric")
  expect_equal(ncol(myData), 35)
  expect_equal(nrow(myData), 4)

})


test_that("Fix issue #50", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  mydsws$chunkLimit <- 50L
  myData <- mydsws$timeSeriesListRequest(instrument="WSUS1",datatype = c("WC18198","WC01084"),startDate = "-2Y",endDate="0Y",frequency = "Y")
  expect_equal(length(myData), 2)
  expect_true(ncol(myData[[1]]) > 900)

})
