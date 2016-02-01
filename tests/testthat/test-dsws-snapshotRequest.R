##############################################################################################

context("classConstructor.R : test of snapshotRequest method")

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()



  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "P",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 2)

})


##############################################################################################

test_that("test of simple snapshot request for price datatype with absolute dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "P",
                                   requestDate = as.Date("2015-12-09"))

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 2)

})


##############################################################################################

test_that("test of simple snapshot request with single datatypes that return strings", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "NAME",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "character")
  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 2)


})

##############################################################################################

test_that("test of simple snapshot request with datatypes that return dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "EPSFD",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "Date")
  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 2)


})

##############################################################################################

test_that("test of simple snapshot request with two datatypes that return name and dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = c("NAME", "EPSFD"),
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,3], "Date")
  expect_equal(myData[1,2], "ASSOCIATED BRIT.FOODS")

  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 3)

})


##############################################################################################

test_that("test of chunked snapshot request with two datatypes that return name and dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  symbolList <- mydsws$listRequest(instrument = "LFTSE100",
                                   datatype = "MNEM",
                                   requestDate = "0D")


  myData <- mydsws$snapshotRequest(instrument = symbolList[,2],
                                   datatype = c("NAME", "EPSFD"),
                                   requestDate = "0D")

  # Get the same data with chunking
  mydsws <- dsws$new()
  mydsws$chunkLimit <- 25L
  myDataChunked <- mydsws$snapshotRequest(instrument = symbolList[,2],
                                          datatype = c("NAME", "EPSFD"),
                                          requestDate = "0D")

  expect_identical(myData, myDataChunked)
})


##############################################################################################

test_that("test of equity risk premium", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("USASERP", "UKASERP", "EKASERP", "JPASERP", "WDASERP"),
                                   expression = "XXXX",
                                   requestDate =  as.Date("2016-01-15"))

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_equal(nrow(myData), 5)
  expect_equal(ncol(myData), 2)
  expect_false(is.na(myData[1,2]))

})

##############################################################################################

test_that("test of requesting complex expression", {
# Actually this request gets a $$"ER","E21B","INVALID CODE..." error from Datastream
    if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myData <- mydsws$snapshotRequest(instrument = c("USTBI10" ,
                                                  "BMCN10Y(RY)-MLCNGIL(RY)",
                                                  "BMFR10Y(RY)-MLFRGIL(RY)",
                                                  "BMUK10Y(RY)-MLUKGIL(RY)",
                                                  "BMAU10Y(RY)-MLAUGIL(RY)",
                                                  "BMJP10Y(RY)-MLG0YIY(RY)"),
                                   expression = "XXXX",
                                   requestDate =  as.Date("2016-01-15"))

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "character")
  expect_equal(nrow(myData), 6)
  expect_equal(ncol(myData), 2)
  expect_false(is.na(myData[1,2]))
  # Need a test that the cells do not contain $$"ER"

})
