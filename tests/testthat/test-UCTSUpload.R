
#------------------------------------------------------------------------------
test_that(" Test the post string generation code", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()



  testData <- xts::xts(x = c(1, 2.2, 3.12345, 4.5), order.by = as.Date(c("2014-04-22","2014-04-23","2014-04-24","2014-04-25")))

  sPost <- UCTSUpload(TSCode = "TSTEST01",
                      MGMTGroup = "TEST",
                      freq = "D",
                      seriesName = "Automatic Upload Test",
                      Units = "",
                      Decimals = 2,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "MID",
                      Carry = "NO",
                      PrimeCurr = "",
                      tsData = testData)

  sExpected <-  structure(TRUE, error = "")

  expect_equal(sPost, sExpected)

  #Try uploading a real dataset
  sPost <- UCTSUpload(TSCode = "TSTEST01",
                      MGMTGroup = "TEST",
                      freq = "D",
                      seriesName = "Automatic Upload Test",
                      Units = "par",
                      Decimals = 2,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "MID",
                      Carry = "NO",
                      PrimeCurr = "U$",
                      tsData = testData)


  sExpected <-  structure(TRUE, error = "")

  expect_equal(sPost, sExpected)

})



#------------------------------------------------------------------------------
test_that(" Test uploading inside dsws object", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()



  testData <- xts::xts(x = c(1, 2.2, 3.12345, 4.5), order.by = as.Date(c("2014-04-22","2014-04-23","2014-04-24","2014-04-25")))

  mydsws <- dsws$new()

  sPost <- mydsws$UCTSUpload(TSCode = "TSTEST01",
                      MGMTGroup = "TEST",
                      freq = "D",
                      seriesName = "Automatic Upload Test",
                      Units = "",
                      Decimals = 2,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "MID",
                      Carry = "NO",
                      PrimeCurr = "",
                      tsData = testData)

  expect_equal(sPost, TRUE)
  expect_equal(mydsws$errorlist, NULL)


})


#------------------------------------------------------------------------------
test_that("Test a dataset with an NaN in it", {
  skip_on_cran()

  testData <- xts::xts(x = c(4.445, 4.121, -30754.896, 0.0001, NaN, NA, "TEXT"),
                  order.by = as.Date(c("2013-01-01", "2013-02-01", "2013-03-01", "2013-04-01", "2013-05-01", "2013-06-01", "2013-07-01")))

  sPost <- DatastreamDSWS2R:::.getTimeseries(testData,"M",2)

  sExpected <-  structure(c(4.44, 4.12, -30754.9, 0, NA, NA, NA), class = "AsIs")

  expect_equal(sPost , sExpected)

})



#------------------------------------------------------------------------------
test_that("Test a dataset with an NaN, NA and a large value in it", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()



  testData <- xts::xts(x = c(1, 2.2, 3.12345, 14.5, NaN), order.by = as.Date(c("2013-01-01","2013-02-01","2013-03-01","2013-04-01","2013-05-01")))

  sPost <- DatastreamDSWS2R:::.getTimeseries(testData,"M",2)

  sExpected <- structure(c(1.00,2.20,3.12,14.50,NA), class = "AsIs")

  expect_equal(sPost , sExpected)

  sPost <- UCTSUpload(TSCode = "TSTEST01",
                      MGMTGroup = "TEST",
                      freq = "M",
                      seriesName = "Automatic Upload Test",
                      Units = "par",
                      Decimals = 2,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "MID",
                      Carry = "NO",
                      PrimeCurr = "U$",
                      tsData = testData)


  sExpected <-  structure(TRUE, error = "")

  expect_equal(sPost , sExpected)

})


#------------------------------------------------------------------------------
test_that("Try uploading a real dataset", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()


  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))
  fTest <- head(f$First,10)

  # Test getTimeseries for the first 10 points
  tData <- DatastreamDSWS2R:::.getTimeseries(Data = fTest, freq = "D", digits = 4)
  tDataExpected <- structure(c(0.8559,NA,NA,NA,0.8579,0.8512,0.8599,NA,NA,0.8596,NA,0.8393,0.8406,0.8274,0.8505,0.8444), class = "AsIs")
  expect_equal(tData , tDataExpected)

  #Try a round trip and check if data is the same
  sPost <- UCTSUpload(TSCode = "TSTEST01",
                      MGMTGroup = "TEST",
                      freq = "D",
                      seriesName = "Automatic Upload Test",
                      Units = "",
                      Decimals = 3,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "END",
                      Carry = "NO",
                      PrimeCurr = "",
                      tsData = fTest)
  expect_equal(sPost , structure(TRUE, error = ""))  #Failed to upload

  #Now lets download the data
  mydsws <- dsws$new()
  aTS <- mydsws$timeSeriesRequest(instrument = "TSTEST01",
                            datatype = "",
                            startDate = zoo::index(xts::first(fTest)),
                            endDate = zoo::index(xts::last(fTest)),
                            frequency = "D")

  #So success is aTS is the same as f$First

  xResult <- cbind(round(fTest, digits = 3), aTS)  # Need to round to the same number of digits as in upload

  colnames(xResult) <- c("Sent", "Got")
  expect_equal(!FALSE %in% as.vector(xResult$Sent == xResult$Got), TRUE)

})


#------------------------------------------------------------------------------
test_that("Try uploading a real dataset with GBP isocode currency", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()


  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))
  fTest <- head(f$First, 10)

  # Test getTimeseries for the first 10 points
  tData <- DatastreamDSWS2R:::.getTimeseries(Data = fTest, freq = "D", digits = 4)
  tDataExpected <- structure(c(0.8559,NA,NA,NA,0.8579,0.8512,0.8599,NA,NA,0.8596,NA,0.8393,0.8406,0.8274,0.8505,0.8444), class = "AsIs")
  expect_equal(tData , tDataExpected)

  #Try a round trip and check if data is the same
  sPost <- UCTSUpload(TSCode = "TSTEST99",
                      MGMTGroup = "TEST",
                      freq = "D",
                      seriesName = "Automatic Upload Test",
                      Units = "",
                      Decimals = 3,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "END",
                      Carry = "NO",
                      PrimeCurr = "GBP",
                      tsData = f)
  expect_equal(sPost , structure(TRUE, error = ""))

  #Now lets download the data
  mydsws <- dsws$new()
  aTS <- mydsws$timeSeriesRequest(instrument = "TSTEST01",
                                datatype = "",
                            startDate = zoo::index(xts::first(fTest)),
                            endDate = zoo::index(xts::last(fTest)),
                            frequency = "D")

  #So success is aTS is the same as f$First

  xResult <- cbind(round(fTest,digits = 3), aTS)  # Need to round to the same number of digits as in upload

  colnames(xResult) <- c("Sent","Got")
  expect_equal(!FALSE %in% as.vector(xResult$Sent == xResult$Got), TRUE)

})


#------------------------------------------------------------------------------
test_that("Error when uploading invalid 4 digit currency", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()


  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))
  fTest <- head(f$First,10)

  # Test getTimeseries for the first 10 points
  tData <- DatastreamDSWS2R:::.getTimeseries(Data = fTest, freq = "D", digits = 4)
  tDataExpected <- structure(c(0.8559,NA,NA,NA,0.8579,0.8512,0.8599,NA,NA,0.8596,NA,0.8393,0.8406,0.8274,0.8505,0.8444), class = "AsIs")
  expect_equal(tData , tDataExpected)


  #Try a round trip and check if data is the same
  expect_error(UCTSUpload(TSCode = "TSTEST01",
                      MGMTGroup = "TEST",
                      freq = "D",
                      seriesName = "Automatic Upload Test",
                      Units = "",
                      Decimals = 3,
                      ActPer = "Y",
                      freqConversion = "END",
                      Alignment = "END",
                      Carry = "NO",
                      PrimeCurr = "GBPS",
                      tsData = fTest),
               "Invalid currency.  Should be either 3 digit ISO code or Datastream code")

})

#------------------------------------------------------------------------------
test_that("Error when uploading invalid 2 digit currency", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))
  fTest <- head(f$First,10)

  # Test getTimeseries for the first 10 points
  tData <- DatastreamDSWS2R:::.getTimeseries(Data = fTest, freq = "D", digits = 4)
  tDataExpected <- structure(c(0.8559,NA,NA,NA,0.8579,0.8512,0.8599,NA,NA,0.8596,NA,0.8393,0.8406,0.8274,0.8505,0.8444), class = "AsIs")
  expect_equal(tData , tDataExpected)

  #Try a round trip and check if data is the same
  expect_error(UCTSUpload(TSCode = "TSTEST01",
                          MGMTGroup = "TEST",
                          freq = "D",
                          seriesName = "Automatic Upload Test",
                          Units = "",
                          Decimals = 3,
                          ActPer = "Y",
                          freqConversion = "END",
                          Alignment = "END",
                          Carry = "NO",
                          PrimeCurr = "ZZ",
                          tsData = fTest),
               "Invalid currency.  Should be an Datastream code in table currencyDS2ISO.")

})

#------------------------------------------------------------------------------
test_that("Error when uploading invalid 3 digit currency", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))
  fTest <- head(f$First,10)

  # Test getTimeseries for the first 10 points
  tData <- DatastreamDSWS2R:::.getTimeseries(Data = fTest, freq = "D", digits = 4)
  tDataExpected <- structure(c(0.8559,NA,NA,NA,0.8579,0.8512,0.8599,NA,NA,0.8596,NA,0.8393,0.8406,0.8274,0.8505,0.8444), class = "AsIs")
  expect_equal(tData , tDataExpected)


  #Try a round trip and check if data is the same
  expect_error(UCTSUpload(TSCode = "TSTEST01",
                          MGMTGroup = "TEST",
                          freq = "D",
                          seriesName = "Automatic Upload Test",
                          Units = "",
                          Decimals = 3,
                          ActPer = "Y",
                          freqConversion = "END",
                          Alignment = "END",
                          Carry = "NO",
                          PrimeCurr = "ZZZ",
                          tsData = fTest),
               "Invalid currency.  Should be an ISO code in table currencyDS2ISO.")

})

#------------------------------------------------------------------------------
test_that("Check timing of daily uploads which start on the weekend", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  # Start date is a Tuesday
  fTest <- xts::xts(1:10,
                    order.by = as.Date(c(
                      "2024-01-09",
                      "2024-01-10",
                      "2024-01-11",
                      "2024-01-12",
                      "2024-01-15",
                      "2024-01-16",
                      "2024-01-17",
                      "2024-01-18",
                      "2024-01-19",
                      "2024-01-22")))

  ret <- UCTSUpload(TSCode = "TSTESTD2",
                    MGMTGroup = "TEST",
                    freq = "D",
                    seriesName = "Automatic Upload Test",
                    Units = "",
                    Decimals = 3,
                    ActPer = "Y",
                    freqConversion = "END",
                    Alignment = "END",
                    Carry = "NO",
                    PrimeCurr = "",
                    tsData = fTest)

  Sys.sleep(3)

  mydsws <- dsws$new()
  fRet <- mydsws$timeSeriesRequest(instrument = "TSTESTD2",
                    datatype = "",
                    startDate = as.Date("2024-01-09"),
                    endDate = as.Date("2024-01-22"),
                    frequency = "D")

  fM <- xts::merge.xts(fTest, fRet)
  fM$Diff <- fM[,1] - fM[,2]

  testthat::expect_false(any(fM$Diff != 0))

  rm(fTest, fRet, fM, ret)

  # Start date is a Friday
  fTest <- xts::xts(1:10,
                    order.by = as.Date(c(
                      "2024-01-12",
                      "2024-01-15",
                      "2024-01-16",
                      "2024-01-17",
                      "2024-01-18",
                      "2024-01-19",
                      "2024-01-22",
                      "2024-01-23",
                      "2024-01-24",
                      "2024-01-25")))

  ret <- UCTSUpload(TSCode = "TSTESTD5",
                    MGMTGroup = "TEST",
                    freq = "D",
                    seriesName = "Automatic Upload Test",
                    Units = "",
                    Decimals = 3,
                    ActPer = "Y",
                    freqConversion = "END",
                    Alignment = "END",
                    Carry = "NO",
                    PrimeCurr = "",
                    tsData = fTest)

  Sys.sleep(3)

  fRet <- mydsws$timeSeriesRequest(instrument = "TSTESTD5",
                                   datatype = "",
                                   startDate = as.Date("2024-01-12"),
                                   endDate = as.Date("2024-01-25"),
                                   frequency = "D")

  fM <- xts::merge.xts(fTest, fRet)
  fM$Diff <- fM[,1] - fM[,2]

  testthat::expect_false(any(fM$Diff != 0))

  rm(fTest, fRet, fM, ret)


  # Start date is a Saturday, but have weekends
  fTest <- xts::xts(1:10,
                    order.by = as.Date(c(
                      "2024-01-13",
                      "2024-01-15",
                      "2024-01-16",
                      "2024-01-17",
                      "2024-01-18",
                      "2024-01-19",
                      "2024-01-22",
                      "2024-01-23",
                      "2024-01-24",
                      "2024-01-25")))

  ret <- UCTSUpload(TSCode = "TSTESTD6",
                    MGMTGroup = "TEST",
                    freq = "D",
                    seriesName = "SaturdayStart",
                    Units = "",
                    Decimals = 3,
                    ActPer = "Y",
                    freqConversion = "END",
                    Alignment = "END",
                    Carry = "NO",
                    PrimeCurr = "",
                    tsData = fTest)

  Sys.sleep(3)

  fRet <- mydsws$timeSeriesRequest(instrument = "TSTESTD6",
                                   datatype = "",
                                   startDate = as.Date("2024-01-12"),
                                   endDate = as.Date("2024-01-25"),
                                   frequency = "D")

  fM <- xts::merge.xts(fTest, fRet)
  fM$Diff <- fM[,1] - fM[,2]

  testthat::expect_false(any(fM$Diff[-(1:2)] != 0))
  testthat::expect_true(zoo::index(fM)[1] == as.Date("2024-01-12"))
  rm(fTest, fRet, fM, ret)


  # Start date is a Sunday, but have weekends
  fTest <- xts::xts(1:10,
                    order.by = as.Date(c(
                      "2024-01-14",
                      "2024-01-15",
                      "2024-01-16",
                      "2024-01-17",
                      "2024-01-18",
                      "2024-01-19",
                      "2024-01-22",
                      "2024-01-23",
                      "2024-01-24",
                      "2024-01-25")))

  ret <- UCTSUpload(TSCode = "TSTESTD7",
                    MGMTGroup = "TEST",
                    freq = "D",
                    seriesName = "SundayStart",
                    Units = "",
                    Decimals = 3,
                    ActPer = "Y",
                    freqConversion = "END",
                    Alignment = "END",
                    Carry = "NO",
                    PrimeCurr = "",
                    tsData = fTest)

  Sys.sleep(3)

  fRet <- mydsws$timeSeriesRequest(instrument = "TSTESTD7",
                                   datatype = "",
                                   startDate = as.Date("2024-01-12"),
                                   endDate = as.Date("2024-01-25"),
                                   frequency = "D")
  fM <- xts::merge.xts(fTest, fRet)
  fM$Diff <- fM[,1] - fM[,2]

  testthat::expect_true(zoo::index(fM)[1] == as.Date("2024-01-12"))
  testthat::expect_false(any(fM$Diff[-(1:2)] != 0))

  rm(fTest, fRet, fM, ret)

})

