##############################################################################################

context("UCTSAppend.R : test of appending timeSeries")







#------------------------------------------------------------------------------
test_that("Try appending a real dataset", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()  # cran is not set up to access Datastream

  require(xts)
  load(file.path(testthat::test_path(), "testData/f.RData"))

  fTest<-head(f$First,10)

  #Try a round trip and check if data is the same
  sPost <- UCTSAppend(TSCode="TSTEST01",
                      MGMTGroup="TEST",
                      freq = "D",
                      seriesName="Automatic Upload Test",
                      Units="",
                      Decimals=3,
                      ActPer="Y",
                      freqConversion="END",
                      Alignment="END",
                      Carry="NO",
                      PrimeCurr="",
                      tsData=fTest)

  expect_equal(sPost , structure(TRUE, error = ""))  #Failed to upload

  #Now lets download the data
  dwei <- getDataStream(User=Sys.getenv("DatastreamUsername"), Pass=Sys.getenv("DatastreamPassword"))
  sGet <- timeSeriesRequest(dwei = dwei,
                            DSCodes = "TSTEST01",
                            Instrument = "",
                            startDate = index(first(fTest)),
                            endDate = index(last(fTest)),
                            frequency = "D",
                            sStockList = sTest,
                            aTimeSeries = aTS,
                            verbose = FALSE)

  #So success is aTS is the same as f$First

  xResult <- cbind(round(fTest,digits=3),aTS)  # Need to round to the same number of digits as in upload

  colnames(xResult) <- c("Sent","Got")
  expect_equal(!FALSE %in% as.vector(xResult$Sent==xResult$Got), TRUE)
})


#------------------------------------------------------------------------------
test_that("Appending two more rows to UCTS", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()  # cran is not set up to access Datastream

  # Now append another two rows
  load(file.path(testthat::test_path(), "testData/f.RData"))

  fTestAppend <- f$First[11:12,]
  fExpected <- f$First[1:12,]
  #Try a round trip and check if data is the same
  sPost <- UCTSAppend(TSCode="TSTEST01",
                      MGMTGroup="TEST",
                      freq = "D",
                      seriesName="Automatic Upload Test",
                      Units="",
                      Decimals=3,
                      ActPer="Y",
                      freqConversion="END",
                      Alignment="END",
                      Carry="NO",
                      PrimeCurr="",
                      tsData=fTestAppend)

  expect_equal(sPost , structure(TRUE, error = ""))  #Failed to upload

  #Now lets download the data
  dwei <- getDataStream(User=Sys.getenv("DatastreamUsername"), Pass=Sys.getenv("DatastreamPassword"))
  sGet <- timeSeriesRequest(dwei = dwei,
                            DSCodes = "TSTEST01",
                            Instrument = "",
                            startDate = index(first(fExpected)),
                            endDate = index(last(fExpected)),
                            frequency = "D",
                            sStockList = sTest,
                            aTimeSeries = aTS,
                            verbose = FALSE)

  # Need to round to the same number of digits as in upload
  xResult <- cbind(round(fExpected, digits=3),aTS)

  colnames(xResult) <- c("Sent", "Got")
  expect_equal(!FALSE %in% as.vector(xResult$Sent == xResult$Got), TRUE)

})


