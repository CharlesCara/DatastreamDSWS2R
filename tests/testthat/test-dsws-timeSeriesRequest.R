##############################################################################################

context("classConstructor.R : test of timeSeriesRequest method")

suppressPackageStartupMessages(require(xts))
##############################################################################################

test_that("test of simple timeseries request with relative dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  xtsData <- mydsws$timeSeriesRequest(instrument = "MKS",
                                      datatype = "MV",
                                      startDate = "-30D",
                                      endDate = "-0D",
                                      frequency = "D")

  expect_is(xtsData, "xts")
  expect_equal(colnames(xtsData), "MKS")

  rm(mydsws, xtsData)
})

##############################################################################################

test_that("test of two stock timeseries request with relative dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  xtsData <- mydsws$timeSeriesRequest(instrument = c("MKS","RIO"),
                                      datatype = "MV",
                                      startDate = "-30D",
                                      endDate = "-0D",
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)
})


##############################################################################################

test_that("test of simple timeseries request with absolute dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  xtsData <- mydsws$timeSeriesRequest(instrument = "MKS",
                                      datatype = "MV",
                                      startDate = as.Date("2014-10-31"),
                                      endDate = as.Date("2014-11-15"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})


##############################################################################################

test_that("test of two stock timeseries request with absolute dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  xtsData <- mydsws$timeSeriesRequest(instrument = c("MKS","RIO"),
                                      datatype = "MV",
                                      startDate = as.Date("2014-10-31"),
                                      endDate = as.Date("2014-11-15"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

#######################################################################################
test_that("test of the instrument being an expression with economic data", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  xtsData <- mydsws$timeSeriesRequest(instrument = "PCH#(USCONPRCF,12M)",
                                      datatype = "",
                                      startDate = as.Date("31/12/1960", "%d/%m/%Y"),
                                      endDate = as.Date("31/12/2014", "%d/%m/%Y"),
                                      frequency = "M")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})
#######################################################################################
test_that("test of the instrument being an expression with economic data but call with wrong periodicity", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  xtsData <- mydsws$timeSeriesRequest(instrument = "PCH#(USCONPRCF,12M)",
                                      datatype = "",
                                      startDate = as.Date("31/12/1960", "%d/%m/%Y"),
                                      endDate = as.Date("31/12/2014", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

##############################################################################
test_that("test of three stock request", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("SAB", "RIO", "MKS"),
                                      datatype = "P",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date("31/12/2014", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

##############################################################################################
test_that("test of multi stock timeseries request with an expression as the datatype", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("SAB", "RIO", "MKS"),
                                      expression = "PCH#(XXXX,5D)",
                                      startDate = as.Date("31/10/2015", "%d/%m/%Y"),
                                      endDate = as.Date("15/11/2015", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)


})

#############################################################
test_that("test of download worldscope data - this data is numeric", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("@AAPL"),
                                      expression = "XXXX(WC01001A)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date('24/01/2012', '%d/%m/%Y'),
                                      frequency = "D")

  expect_is(xtsData, "xts")
  myValues <- coredata(xtsData)
  colnames(myValues) <- NULL
  expect_equal(myValues[1:2,1], c(28237000, NA) )
  rm(mydsws, xtsData)

})



#############################################################
test_that("test of download worldscope data - this data is Dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("@AAPL"),
                                      expression = "XXXX(WC05905A)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date('24/01/2012', '%d/%m/%Y'),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  myValues <- coredata(xtsData)
  colnames(myValues) <- NULL
  expect_is(myValues[1,1], "character")
  expect_equal(myValues[1:2,1], c("2011-10-18", NA) )  # Date to be decoded

  rm(mydsws, xtsData)

})

#############################################################
test_that("test of download worldscope data for multiple stocks", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("@AAPL", "MKS"),
                                      expression = "XXXX(WC05905A)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date('24/01/2012', '%d/%m/%Y'),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

#############################################################
test_that("test of Datastream expression eg 045E(XXXX)", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("@AAPL", "MKS"),
                                      expression = "045E(XXXX)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date('24/01/2012', '%d/%m/%Y'),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

#############################################################
test_that("test of ISIN code and expression that was not being returned", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  mydsws$setLogging(3)
  xtsData <- mydsws$timeSeriesRequest(instrument = c("JP3111200006","JP3802400006"),
                                      expression = "(XXXX(SAL1FD12)/XXXX(SAL1TR12))-1.00",
                                      startDate = as.Date("1996-01-01"),
                                      endDate = as.Date("2016-01-20"),
                                      frequency = "M")

  expect_is(xtsData, "xts")
  expect_equal(ncol(xtsData), 2L)
  rm(mydsws, xtsData)

})





#############################################################
#
# test_that("test of selecting stocks via ISIN codes with missing (NA) codes and expressions", {
#   if(is.null(options()$Datastream.Username)){
#     skip("Username not available")
#   }
#   skip_on_cran()
#
#   mydsws <- dsws$new()
#   xtsData <- mydsws$timeSeriesRequest(instrument = c("NO0010716582",NA,"SE0005999836",NA,"BMG454221059"),
#                                       datatype = "P",
#                                       startDate = as.Date("01/06/2015", "%d/%m/%Y"),
#                                       endDate = as.Date("01/08/2015", "%d/%m/%Y"),
#                                       frequency = "D")
#
#   expect_is(xtsData, "xts")
#   expect_equal(ncol(xts), 5L)
#
#   rm(mydsws, xtsData)
#
# })
#############################################################

test_that("test of selecting stocks via ISIN codes with missing (NA) codes and expressions", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c("NO0010716582",NA,"SE0005999836",NA,"BMG454221059"),
                                      expression = "(XXXX(EPS1FD12)/XXXX(EPS1TR12))-1.00",
                                      startDate = as.Date("01/06/2015", "%d/%m/%Y"),
                                      endDate = as.Date("01/08/2015", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")
  expect_equal(ncol(xtsData), 5L)

  rm(mydsws, xtsData)

})


#############################################################

test_that("test of selecting stocks via ISIN codes with the first code missing and expressions", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument = c(NA,"NO0010716582","SE0005999836",NA,"BMG454221059"),
                                      expression = "(XXXX(EPS1FD12)/XXXX(EPS1TR12))-1.00",
                                      startDate = as.Date("01/06/2015", "%d/%m/%Y"),
                                      endDate = as.Date("01/08/2015", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")
  expect_equal(ncol(xtsData), 5L)

  rm(mydsws, xtsData)

})

#############################################################

test_that("further test of missing stocks ", {
  # In this test, nothing is returned from Datastream for the third stock.  My thought is that
  # the correct response should be an xts with a column per instrument
  # and a single row (dated endDate) that has NA's in it.

  if(is.null(options()$Datastream.Username)){
  skip("Username not available")
}
skip_on_cran()

mydsws <- dsws$new()
xtsData <- mydsws$timeSeriesRequest(instrument = c("CA91911K1021","US8552441094","VEV0019410A9",
                                                   "US5007541064","US4385161066"),
                                    expression = "XXXX(BPS1TR12)/XXXX",
                                    startDate = as.Date("2016-01-30"),
                                    endDate = as.Date("2016-02-01"),
                                    frequency = "D")

expect_is(xtsData, "xts")
expect_equal(ncol(xtsData), 5L)
expect_equal(nrow(xtsData), 2L)
expect_true(TRUE %in% is.na(xtsData[1,]))

rm(mydsws, xtsData)

})

#############################################################
test_that("test of Japanese ISIN with PE", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument =  c("JP3677200002"),
                                      expression = "XXXX(PE)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date("01/08/2015", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

##############################################################################################
test_that("test of ISIN returns Failure, status code 2, message $$\"ER\", 0904, NO DATA AVAILABLE", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument =  c("CA54042L1004"),
                                      expression = "XXXX(PE)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date("01/08/2015", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

##############################################################################################
# This is a mixture of NO DATA AVAILABLE and valid data
#
test_that("test of ISIN returns mixture of NO DATA AVAILABLE and valid data", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()
  xtsData <- mydsws$timeSeriesRequest(instrument =  c("CA54042L1004", "JP3677200002"),
                                      expression = "XXXX(PE)",
                                      startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                      endDate = as.Date("01/08/2015", "%d/%m/%Y"),
                                      frequency = "D")

  expect_is(xtsData, "xts")

  rm(mydsws, xtsData)

})

##############################################################################################
# This is a test of chunked requests - first make an unchunked one, and then a chunked one
#
test_that("test of chunked timeSeriesRequests with a datatype", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  symbolList <- mydsws$listRequest(instrument = "LFTSE100",
                                   datatype = "MNEM",
                                   requestDate = "0D")

  # Get the unchunked data

  xtsTestData <- mydsws$timeSeriesRequest(instrument =  symbolList[,2],
                                          datatype = "PE",
                                          startDate = as.Date("2011-08-31"),
                                          endDate = as.Date("2011-11-30"),
                                          frequency = "W")
  expect_is(xtsTestData, "xts")
  # Get the same data with chunking
  rm(mydsws)
  mydsws <- dsws$new()
  mydsws$chunkLimit <- 25L

  xtsDataChunked <- mydsws$timeSeriesRequest(instrument =  symbolList[,2],
                                             datatype = "PE",
                                             startDate = as.Date("2011-08-31"),
                                             endDate = as.Date("2011-11-30"),
                                             frequency = "W")

  expect_is(xtsDataChunked, "xts")
  expect_identical(xtsTestData, xtsDataChunked)

  rm(mydsws, xtsTestData, xtsDataChunked, symbolList)
})

##############################################################################################
# This is a test of chunked requests - first make an unchunked one, and then a chunked one
#
test_that("test of chunked timeSeriesRequests with an expression", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  symbolList <- mydsws$listRequest(instrument = "LFTSE100",
                                   datatype = "MNEM",
                                   requestDate = "0D")

  # Get the unchunked data

  xtsTestData <- mydsws$timeSeriesRequest(instrument =  symbolList[,2],
                                          expression = "XXXX(PE)",
                                          startDate = as.Date("2011-08-31"),
                                          endDate = as.Date("2011-11-30"),
                                          frequency = "W")
  expect_is(xtsTestData, "xts")
  # Get the same data with chunking
  rm(mydsws)
  mydsws <- dsws$new()
  mydsws$chunkLimit <- 25L

  xtsDataChunked <- mydsws$timeSeriesRequest(instrument =  symbolList[,2],
                                             expression = "XXXX(PE)",
                                             startDate = as.Date("2011-08-31"),
                                             endDate = as.Date("2011-11-30"),
                                             frequency = "W")

  expect_is(xtsDataChunked, "xts")
  expect_identical(xtsTestData, xtsDataChunked)

  rm(mydsws, xtsTestData, xtsDataChunked, symbolList)
})

##############################################################################################
# This is a test of chunked requests - This is test that large requests strings are chunked correctly
#

test_that("test of chunking timeSeriesRequests due to request string length", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  symbolList <- mydsws$listRequest(instrument = "LFTSE100",
                                   datatype = "MNEM",
                                   requestDate = "0D")

  # Get the unchunked data

  xtsTestData <- mydsws$timeSeriesRequest(instrument =  symbolList[,2],
                                          expression = "XXXX(PE)",
                                          startDate = as.Date("2011-08-31"),
                                          endDate = as.Date("2011-11-30"),
                                          frequency = "W")
  expect_is(xtsTestData, "xts")
  # Get the same data with chunking
  rm(mydsws)
  mydsws <- dsws$new()
  mydsws$requestStringLimit <- 300L

  xtsDataChunked <- mydsws$timeSeriesRequest(instrument =  symbolList[,2],
                                             expression = "XXXX(PE)",
                                             startDate = as.Date("2011-08-31"),
                                             endDate = as.Date("2011-11-30"),
                                             frequency = "W")

  expect_is(xtsDataChunked, "xts")
  expect_identical(xtsTestData, xtsDataChunked)

  rm(mydsws, xtsTestData, xtsDataChunked, symbolList)


})

##############################################################################################
# This is a test of chunked requests - This is test that large requests strings are chunked correctly
#

test_that("test of chunking timeSeriesRequests due to request string length", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  symbolList <- readRDS("testData/G100DSCodes.rds")
  #symbolList <- readRDS("tests/testthat/testData/G100DSCodes.rds")

  # Get the unchunked data

  xtsTestData <- mydsws$timeSeriesRequest(instrument =  symbolList,
                                          expression = "XXXX(DWND)/XXXX(DWSE)*100.00",
                                          startDate = as.Date("1996-01-01"),
                                          endDate = as.Date("2016-01-10"),
                                          frequency = "W")
  expect_is(xtsTestData, "xts")
  expect_true(ncol(xtsTestData) == 426)
  expect_true(nrow(xtsTestData) == 1045)
  rm(mydsws, xtsTestData, symbolList)
})
