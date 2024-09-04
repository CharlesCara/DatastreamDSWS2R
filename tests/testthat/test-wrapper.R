context("test-wrapper - tests of wrapper functions")

##############################################################################################

test_that("staticListRequestSet - array of instruments", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()


  myData <- staticListRequestSet(instrument = c("FTSE100", "S&PCOMP"),
                                             expression = "X",
                                             endDate = Sys.Date(),
                                             frequency = "D")

  expect_is(myData, "data.frame")
  expect_is(myData[2,1], "numeric")
  expect_equal(nrow(myData), 2)
  expect_equal(ncol(myData), 1)

})


test_that("staticListRequestSet - array of instruments", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()


  myData <- staticListRequestSet(instrument = c("LFTSE100"),
                                 expression = "X",
                                 endDate = Sys.Date(),
                                 frequency = "D")

  expect_is(myData, "data.frame")


})


##############################################################################################

test_that("staticListRequestSet - array of instruments", {
  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()


  myData <- staticRequestSet(instrument = c("FTSE100", "S&PCOMP"),
                                         expression = "PCH#(XXXX,1M)",
                                         endDate = Sys.Date(),
                                         frequency = "D",
                                         verbose = FALSE)

  expect_is(myData, "data.frame")


})


