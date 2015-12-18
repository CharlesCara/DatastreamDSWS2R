##############################################################################################

context("classConstructor.R : test of listRequest method")
source("~/.RProfile")
suppressMessages(.First())

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {



  mydsws <- dsws$new()

  myData <- mydsws$listRequest(instrument = "LFTSE100",
                                   datatype = "P",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_more_than(nrow(myData), 99)
  expect_equal(ncol(myData), 2)

})

test_that("test of simple snapshot request for price datatype with absolute dates", {



  mydsws <- dsws$new()

  myData <- mydsws$listRequest(instrument = "LFTSE100",
                               datatype = "P",
                               requestDate = as.Date("2015-10-01"))

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_more_than(nrow(myData), 99)
  expect_equal(ncol(myData), 2)

})


test_that("test of simple snapshot request for price datatype with relative dates", {

  mydsws <- dsws$new()

  myData <- mydsws$listRequest(instrument = "LFTSE100",
                               datatype = "NAME",
                               requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "character")
  expect_more_than(nrow(myData), 99)
  expect_equal(ncol(myData), 2)

})


test_that("test of simple snapshot request for Expression datatype with relative date", {

  mydsws <- dsws$new()

  myData <- mydsws$listRequest(instrument = "LFTSE100",
                               datatype = "458E(XXXX)",
                               requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "character")
  expect_more_than(nrow(myData), 99)
  expect_equal(ncol(myData), 2)

})


test_that("test of simple snapshot request for Date datatype with relative dates", {

  mydsws <- dsws$new()

  myData <- mydsws$listRequest(instrument = "LFTSE100",
                               datatype = "TIME",
                               requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "Date")
  expect_more_than(nrow(myData), 99)
  expect_equal(ncol(myData), 2)

})
