##############################################################################################

context("classConstructor.R : test of timeSeriesListRequest method")
source("~/.RProfile")
suppressMessages(.First())

##############################################################################################

test_that("test of timeseries list request for price datatype with relative dates", {



  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         datatype = "P",
                                         startDate = "-10D",
                                         endDate = "0D",
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(coredata(myData)[1,2], "numeric")
  expect_more_than(ncol(myData), 99)
  expect_equal(nrow(myData), 11)

})

test_that("test of timeseries list request for default datatype with relative dates", {



  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         datatype = "",
                                         startDate = "-10D",
                                         endDate = "0D",
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(coredata(myData)[1,2], "numeric")
  expect_more_than(ncol(myData), 99)
  expect_equal(nrow(myData), 11)

})


test_that("test of timeseries list request for price datatype with absolute dates", {



  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         datatype = "P",
                                         startDate = as.Date("2015-09-27"),
                                         endDate = as.Date("2015-09-30"),
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(coredata(myData)[1,2], "numeric")
  expect_more_than(ncol(myData), 99)
  expect_equal(nrow(myData), 4)

})


test_that("test of timeseries list request for expression with relative date", {

  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                         expression = "110E(XXXX)",
                                         startDate = as.Date("2015-09-27"),
                                         endDate = as.Date("2015-09-30"),
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(coredata(myData)[1,2], "numeric")
  expect_more_than(ncol(myData), 99)
  expect_equal(nrow(myData), 4)

})



test_that("test of timeseries list request for user created list that has a mixture of indices and expressions", {

  mydsws <- dsws$new()

  myData <- mydsws$timeSeriesListRequest(instrument = "L#CROSS",
                                         expression = "",
                                         startDate = as.Date("2015-09-27"),
                                         endDate = as.Date("2015-09-30"),
                                         frequency = "D")

  expect_is(myData, "xts")
  expect_is(coredata(myData)[1,2], "numeric")
  expect_equal(ncol(myData), 35)
  expect_equal(nrow(myData), 4)

})

