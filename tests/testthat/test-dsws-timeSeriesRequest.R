##############################################################################################

context("classConstructor.R : test of timeSeriesRequest method")

suppressPackageStartupMessages(require(xts))

#testDataFolder <- "tests/testthat/testData/"
testDataFolder <- "./testData/"
##############################################################################################

test_that("test of simple timeseries request with relative dates", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  #Load the response rather than hit server
  mydsws$jsonResponseLoadFile <- file.path(testDataFolder, "test-dsws-timeSeriesRequest-test01.json")

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
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  #Load the response rather than hit server
  mydsws$jsonResponseLoadFile <- file.path(testDataFolder, "test-dsws-timeSeriesRequest-test02.json")

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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
test_that("test of selecting stocks via ISIN codes with missing (NA) codes ", {
if(Sys.getenv("DatastreamUsername") == ""){
  skip("Username not available")
}
skip_on_cran()


DSCodes <- c("JP3735400008", NA, "JP3305990008", "JP3117700009")
startDate <- as.Date("1994-12-02")
endDate <- as.Date("2018-01-23")
mydsws <- dsws$new()
xtsRepDates <- mydsws$timeSeriesRequest(instrument = DSCodes,
                                        expression =  "XXXX(WC05905A)",
                                        startDate = startDate,
                                        endDate = endDate,
                                        frequency = "D",
                                        format = "ByInstrument")

naRepDates <- xtsRepDates[ , 2]
expect_false(FALSE %in% is.na(naRepDates))

})
#############################################################

test_that("test of selecting stocks via ISIN codes with the first code missing and expressions", {
  if(Sys.getenv("DatastreamUsername") == ""){
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

  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
  if(Sys.getenv("DatastreamUsername") == ""){
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
# This is a test of setting chunkLimit via options()
#
test_that("test of setting chunkLimit via options()", {

  myChunkLimit <- 123L
  options(Datastream.ChunkLimit = myChunkLimit)

  mydsws <- dsws$new()

  expect_equal(mydsws$chunkLimit, myChunkLimit)

  rm(mydsws, myChunkLimit)
  options(Datastream.ChunkLimit = NULL)
})

##############################################################################################
# This is a test of chunked requests - This is test that large requests strings are chunked correctly
#

test_that("test of chunking timeSeriesRequests due to request string length", {
  if(Sys.getenv("DatastreamUsername") == ""){
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
# This is a test of named instruments requests
#

test_that("test of named instruments, datatypes and expressions", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  myIns <- c("@AAPL", "MKS")
  names(myIns) <- c("a", "b")
  myExp <- "XXXX(P)"
  names(myExp) <- "c"
  myDt <- "P"
  names(myDt) <- "d"



  # Expect
  expect_silent(xtsTestData <- mydsws$timeSeriesRequest(instrument =  myIns,
                                                        expression = myExp,
                                                        startDate = as.Date("2016-01-01"),
                                                        endDate = as.Date("2016-01-10"),
                                                        frequency = "D"))
  expect_is(xtsTestData, "xts")
  expect_true(nrow(xtsTestData) > 1)

  expect_silent(xtsTestData <- mydsws$timeSeriesRequest(instrument =  myIns,
                                                        datatype = myDt,
                                                        startDate = as.Date("2016-01-01"),
                                                        endDate = as.Date("2016-01-10"),
                                                        frequency = "D"))
  expect_is(xtsTestData, "xts")
  expect_true(nrow(xtsTestData) > 1)
  rm(mydsws, myIns, myExp, myDt, xtsTestData)

})

##############################################################################################
# This is a test of chunked requests - This is test that large requests strings are chunked correctly
#

test_that("test of chunking timeSeriesRequests due to request string length", {
  if(Sys.getenv("DatastreamUsername") == ""){
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


##############################################################################################
# This is a test of handling mainframe timeouts
#

test_that("test of handling mainframe timeouts", {

  mydsws <- dsws$new()

  mydsws$jsonResponseLoadFile <- "./testData/TimeoutResponseFull.json"
  # mydsws$jsonResponseLoadFile <- "tests/testthat/testData/TimeoutResponseFull.json"

  # Get the unchunked data

  instCodes <- c("PCH#(US88033G4073(EPS1TR12),3Y)/100.000", "PCH#(US87612E1064(EPS1TR12),3Y)/100.000",
    "PCH#(US87901J1051(EPS1TR12),3Y)/100.000", "PCH#(US8723751009(EPS1TR12),3Y)/100.000",
    "PCH#(US8794338298(EPS1TR12),3Y)/100.000", "PCH#(US8718291078(EPS1TR12),3Y)/100.000",
    "PCH#(US8636671013(EPS1TR12),3Y)/100.000", "PCH#(US8711301007(EPS1TR12),3Y)/100.000",
    "PCH#(US8581222036(EPS1TR12),3Y)/100.000", "PCH#(US7865142084(EPS1TR12),3Y)/100.000",
    "PCH#(US8545021011(EPS1TR12),3Y)/100.000", "PCH#(US8685361037(EPS1TR12),3Y)/100.000",
    "PCH#(US8574771031(EPS1TR12),3Y)/100.000", "PCH#(US7908491035(EPS1TR12),3Y)/100.000",
    "PCH#(US8679141031(EPS1TR12),3Y)/100.000", "PCH#(US8110544025(EPS1TR12),3Y)/100.000",
    "PCH#(US8550301027(EPS1TR12),3Y)/100.000", "PCH#(US8354951027(EPS1TR12),3Y)/100.000",
    "PCH#(US8425871071(EPS1TR12),3Y)/100.000", "PCH#(US87161C5013(EPS1TR12),3Y)/100.000",
    "PCH#(US78442P1066(EPS1TR12),3Y)/100.000", "PCH#(AN8068571086(EPS1TR12),3Y)/100.000",
    "PCH#(US8265521018(EPS1TR12),3Y)/100.000", "PCH#(US8202861022(EPS1TR12),3Y)/100.000",
    "PCH#(US8243481061(EPS1TR12),3Y)/100.000", "PCH#(US8066051017(EPS1TR12),3Y)/100.000",
    "PCH#(US8270561025(EPS1TR12),3Y)/100.000", "PCH#(US8175651046(EPS1TR12),3Y)/100.000",
    "PCH#(US8085131055(EPS1TR12),3Y)/100.000", "PCH#(US80589M1027(EPS1TR12),3Y)/100.000",
    "PCH#(US7864291007(EPS1TR12),3Y)/100.000", "PCH#(US7551115071(EPS1TR12),3Y)/100.000",
    "PCH#(US7504381036(EPS1TR12),3Y)/100.000", "PCH#(US2578671016(EPS1TR12),3Y)/100.000",
    "PCH#(US7753711073(EPS1TR12),3Y)/100.000", "PCH#(US7617631012(EPS1TR12),3Y)/100.000",
    "PCH#(US7591EP1005(EPS1TR12),3Y)/100.000", "PCH#(US7616951056(EPS1TR12),3Y)/100.000",
    "PCH#(US0019201070(EPS1TR12),3Y)/100.000", "PCH#(US7581101000(EPS1TR12),3Y)/100.000",
    "PCH#(US7512773026(EPS1TR12),3Y)/100.000", "PCH#(US7677541044(EPS1TR12),3Y)/100.000",
    "PCH#(US7835491082(EPS1TR12),3Y)/100.000", "PCH#(US7475251036(EPS1TR12),3Y)/100.000",
    "PCH#(US74005P1049(EPS1TR12),3Y)/100.000", "PCH#(US7310951055(EPS1TR12),3Y)/100.000",
    "PCH#(US6951141083(EPS1TR12),3Y)/100.000", "PCH#(US69351T1060(EPS1TR12),3Y)/100.000",
    "PCH#(US6935061076(EPS1TR12),3Y)/100.000")

  xtsTestData <- mydsws$timeSeriesRequest(instrument =  instCodes,
                                          expression = "",
                                          startDate = as.Date("2017-02-01"),
                                          endDate = as.Date("2017-03-03"),
                                          frequency = "D")
  expect_is(xtsTestData, "xts")
  expect_true(ncol(xtsTestData) == 49)
  expect_true(nrow(xtsTestData) == 23)
  rm(mydsws, xtsTestData, instCodes)
})


##############################################################################################
# This is a test of that if a single timeseries returns $$ER: 0904,NO DATA AVAILABLE and no dates
# the dsws returns a zero row xts.
# Issue # 4
# json response of server stored in testData/test-dsws-timeSeriesRequest-test04.json
#


test_that(" test of that if a single timeseries returns $$ER: 0904,NO DATA AVAILABLE", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  # Get the unchunked data

  xtsActData <- mydsws$timeSeriesRequest(instrument = "KO:SGL",
                                      datatype = "AX",
                                      startDate = "2018-05-07",
                                      endDate = "2018-05-09",
                                      frequency = "D")
  expect_is(xtsActData, "xts")
  expect_true(ncol(xtsActData) == 1)
  expect_true(nrow(xtsActData) == 0)
  rm(mydsws, xtsActData)
})

##############################################################################################
# This is a test of that if a we request two timeseries and the first returns $$ER: 0904,NO DATA AVAILABLE
# we get then dsws returns an xts with rows.
# Issue # 4
#


test_that("equest two timeseries and the first returns $$ER: 0904,NO DATA AVAILABLE", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  # Get the unchunked data

  xtsActData <- mydsws$timeSeriesRequest(instrument = c("III", "KO:SGL"),
                                         datatype = "AX",
                                         startDate = "2018-05-03",
                                         endDate = "2018-05-07",
                                         frequency = "D")
  expect_is(xtsActData, "xts")
  expect_true(ncol(xtsActData) == 2)
  expect_true(nrow(xtsActData) == 3)
  rm(mydsws, xtsActData)
})


##############################################################################################
# This is a test of that if a we request two timeseries and the second returns $$ER: 0904,NO DATA AVAILABLE
# we get then dsws returns an xts with rows.
# Issue # 4
#


test_that("test of that if a we request two timeseries and the second returns $$ER: 0904,NO DATA AVAILABLE", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  # Get the unchunked data

  xtsActData <- mydsws$timeSeriesRequest(instrument = c("KO:SGL", "III"),
                                         datatype = "AX",
                                         startDate = "2018-05-03",
                                         endDate = "2018-05-07",
                                         frequency = "D")
  expect_is(xtsActData, "xts")
  expect_true(ncol(xtsActData) == 2)
  expect_true(nrow(xtsActData) == 3)
  rm(mydsws, xtsActData)
})

##############################################################################################
# This is a test of that if a we request two timeseries and both return $$ER: 0904,NO DATA AVAILABLE
# we get then dsws returns an xts with zero rows.
# Issue # 4
#


test_that(" test of that if a single timeseries returns $$ER: 0904,NO DATA AVAILABLE", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()

  # Get the unchunked data

  xtsActData <- mydsws$timeSeriesRequest(instrument = c("III", "KO:SGL"),
                                         datatype = "AX",
                                         startDate = "2018-05-07",
                                         endDate = "2018-05-09",
                                         frequency = "D")
  expect_is(xtsActData, "xts")
  expect_true(ncol(xtsActData) == 2)
  expect_true(nrow(xtsActData) == 0)
  rm(mydsws, xtsActData)
})

