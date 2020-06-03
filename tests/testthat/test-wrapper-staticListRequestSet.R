context("wrapper.R : staticListRequestSet function")

test_that("test of myStaticRequestSet with a date returned", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

    expect_silent(
    adf <- DatastreamDSWS2R:::myStaticRequestSet(mydsws = DatastreamDSWS2R::dsws$new(),
                                                 instrument = "U:SNAP",
                                                 iExpression = "BDATE",
                                                 endDate = Sys.Date(),
                                                 frequency = "D"))
  expect_is(adf, "data.frame")
  expect_is(adf[1, 1], "Date")
})

test_that("test of staticListRequestSet with a date", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  adf <- DatastreamDSWS2R::staticListRequestSet(mydsws = DatastreamDSWS2R::dsws$new(),
                                                instrument = "U:SNAP", expression = "BDATE",
                                                endDate = Sys.Date(), frequency = "D")
  expect_is(adf[1, 1], "Date")
})


test_that("test of staticListRequestSet with a character and a date", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  adf <- DatastreamDSWS2R::staticListRequestSet(mydsws = DatastreamDSWS2R::dsws$new(),
                                                instrument = "U:SNAP", expression = c("NAME", "BDATE"),
                                                endDate = Sys.Date(), frequency = "D")
  expect_is(adf[1, 1], "character")
  expect_is(adf[1, 2], "Date")
})


test_that("test of staticListRequestSet with a number and a date", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  adf <- DatastreamDSWS2R::staticListRequestSet(mydsws = DatastreamDSWS2R::dsws$new(),
                                                instrument = "U:SNAP", expression = c("P", "BDATE"),
                                                endDate = Sys.Date(), frequency = "D")
  expect_is(adf[1, 1], "numeric")
  expect_is(adf[1, 2], "Date")
})


test_that("test of staticRequestSet with a two timeseries requests", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  Expressions <- c("PCH#(X,+1D)",
                   "PCH#(X,-1W)")

  Codes <- c("MSACWF$(RI)/JPMGIU$(RI)",
             "TOTMKUS(RI)/BMUS10Y(RI)")

  adf <- DatastreamDSWS2R::staticRequestSet(mydsws = DatastreamDSWS2R::dsws$new(),
                                            instrument = Codes,
                                            expression = Expressions,
                                            endDate = Sys.Date()-1, frequency = "D")
  expect_is(adf[1, 1], "numeric")
  expect_is(adf, "data.frame")
})


test_that("test of staticRequestSet with a static and  timeseries requests", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  Expressions <- c("NAME",
                   "PCH#(X,+1D)",
                   "PCH#(X,-1W)")

  Codes <- c("MSACWF$(RI)/JPMGIU$(RI)",
             "TOTMKUS(RI)/BMUS10Y(RI)")

  adf <- DatastreamDSWS2R::staticRequestSet(mydsws = DatastreamDSWS2R::dsws$new(),
                                            instrument = Codes,
                                            expression = Expressions,
                                            endDate = Sys.Date()-1, frequency = "D")
  expect_equal(adf[1, 1], NA)
  expect_is(adf[1, 2], "numeric")
  expect_is(adf, "data.frame")
})

