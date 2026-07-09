##############################################################################################
#------------------------------------------------------------------------------
test_that("Try appending a real dataset", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()  # cran is not set up to access Datastream


  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))

  fTest <- head(f$First,10)

  #Try a round trip and check if data is the same
  expect_warning({
  sPost <- UCTSAppend(TSCode = "TSTEST01",
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
  }, regexp = "UCTSAppend is deprecated and will be removed in a future release.")
  expect_equal(sPost , structure(TRUE, error = ""))  #Failed to upload

  #Now lets download the data
  mydsws <- dsws$new()
  aTS <- mydsws$timeSeriesRequest(instrument = "TSTEST01",
                                  startDate = zoo::index(xts::first(fTest)),
                                  endDate = zoo::index(xts::last(fTest)),
                                  frequency = "D")

  #So success is aTS is the same as f$First

  xResult <- cbind(round(fTest,digits = 3), aTS)  # Need to round to the same number of digits as in upload

  colnames(xResult) <- c("Sent","Got")
  expect_equal(!FALSE %in% as.vector(xResult$Sent == xResult$Got), TRUE)
})


#------------------------------------------------------------------------------
test_that("Appending two more rows to UCTS", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()  # cran is not set up to access Datastream

  # Now append another two rows
  f <- readRDS(file = file.path(testthat::test_path(), "testData/f.RDS"))

  fTestAppend <- f$First[11:12,]
  fExpected <- f$First[1:12,]
  #Try a round trip and check if data is the same
  expect_warning({
    sPost <- UCTSAppend(TSCode = "TSTEST01",
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
                      tsData = fTestAppend)
  }, regexp = "UCTSAppend is deprecated and will be removed in a future release.")

  expect_equal(sPost , structure(TRUE, error = ""))  #Failed to upload

  #Now lets download the data
  mydsws <- dsws$new()
  aTS <- mydsws$timeSeriesRequest(instrument = "TSTEST01",
                                  startDate = zoo::index(xts::first(fExpected)),
                                  endDate = zoo::index(xts::last(fExpected)),
                                  frequency = "D")


  # Need to round to the same number of digits as in upload
  xResult <- cbind(round(fExpected, digits = 3), aTS)

  colnames(xResult) <- c("Sent", "Got")
  expect_equal(!FALSE %in% as.vector(xResult$Sent == xResult$Got), TRUE)

})


