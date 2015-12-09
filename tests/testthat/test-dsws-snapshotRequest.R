##############################################################################################

context("classConstructor.R : test of snapshotRequest method")
source("~/.RProfile")
suppressMessages(.First())

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {



  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "P",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,1], "numeric")
  expect_equal(length(myData[,1]), 3)

})


##############################################################################################

test_that("test of simple snapshot request for price datatype with absolute dates", {
  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "P",
                                   requestDate = as.Date("2015-12-09"))

  expect_is(myData, "data.frame")
  expect_is(myData[1,1], "numeric")
  expect_equal(length(myData[,1]), 3)

})


##############################################################################################

test_that("test of simple snapshot request with single datatypes that return strings", {
  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "NAME",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,1], "character")
  expect_equal(length(myData[,1]), 3)


})

##############################################################################################

test_that("test of simple snapshot request with datatypes that return dates", {

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "EPSFD",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,1], "Date")
  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 2)


})

##############################################################################################

test_that("test of simple snapshot request with two datatypes that return name and dates", {

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = c("NAME", "EPSFD"),
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,3], "Date")
  expect_equal(myData[1,1], "ASSOCIATED BRIT.FOODS")

  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 3)

})



