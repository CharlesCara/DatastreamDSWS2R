##############################################################################################

context("classConstructor.R : test of .buildRequestBundle method")


##############################################################################################

test_that("test of two stocks and a single datatype", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()


  myReq <- mydsws$.buildRequestListBundle(frequency = "D",
                                                   instrument = c("abc", "def"),
                                  datatype = "",
                                  expression = "XXXX(PE)",
                                  isList = FALSE,
                                  startDate= as.Date("2010-01-01"),
                                  endDate = as.Date("2010-12-31"),
                                  kind = 0,
                                  token = "xyz")

  myDT <- list(End = "2010-12-31",
                  Frequency = "D",
                  Kind = 0,
                  Start = "2010-01-01")
  myDTP <- list()

  myDRExp <- list()

  myDRExp[1] <- list(list(DataTypes = myDTP,
                     Date = myDT,
                     Instrument = list(Properties =  NULL,
                                       Value = "ABC(PE)"),
                     Tag = NULL))
  myDRExp[2] <- list(list(DataTypes = myDTP,
                     Date = myDT,
                     Instrument = list(Properties =  NULL,
                                       Value = "DEF(PE)"),
                     Tag = NULL))

  myReqExpected <- list(DataRequests = myDRExp,
                        Properties = list(Properties=NULL),
                        TokenValue = "xyz")


  expect_identical(myReq$requestList, myReqExpected)
  expect_equal(myReq$numDatatype, 1L)
  expect_equal(myReq$numInstrument, 2L)

  })


test_that("test of two stocks and a single expression", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()


  myReq <- mydsws$.buildRequestListBundle(frequency = "D",
                                          instrument = c("abc", "def"),
                                          datatype = "P",
                                          expression = "",
                                          isList = FALSE,
                                          startDate= as.Date("2010-01-01"),
                                          endDate = as.Date("2010-12-31"),
                                          kind = 0,
                                          token = "xyz")

  myDT <- list(End = "2010-12-31",
               Frequency = "D",
               Kind = 0,
               Start = "2010-01-01")
  myDTP <- list(list(Properties =  NULL,
                     Value = "P"))

  myDRExp <- list()

  myDRExp[1] <- list(list(DataTypes = myDTP,
                          Date = myDT,
                          Instrument = list(Properties =  NULL,
                                            Value = "ABC"),
                          Tag = NULL))
  myDRExp[2] <- list(list(DataTypes = myDTP,
                          Date = myDT,
                          Instrument = list(Properties =  NULL,
                                            Value = "DEF"),
                          Tag = NULL))

  myReqExpected <- list(DataRequests = myDRExp,
                        Properties = list(Properties=NULL),
                        TokenValue = "xyz")


  expect_identical(myReq$requestList, myReqExpected)
  expect_equal(myReq$numDatatype, 1L)
  expect_equal(myReq$numInstrument, 2L)

})


test_that("test of two stocks and two datatypes", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  mydsws <- dsws$new()


  myReq <- mydsws$.buildRequestListBundle(frequency = "D",
                                          instrument = c("abc", "def"),
                                          datatype = c("P", "Q"),
                                          expression = "",
                                          isList = FALSE,
                                          startDate= as.Date("2010-01-01"),
                                          endDate = as.Date("2010-12-31"),
                                          kind = 0,
                                          token = "xyz")

  myDT <- list(End = "2010-12-31",
               Frequency = "D",
               Kind = 0,
               Start = "2010-01-01")
  myDTP <- list(list(Properties =  NULL,
                     Value = "P"),
                  list(Properties =  NULL,
                       Value = "Q"))

  myDRExp <- list()

  myDRExp[1] <- list(list(DataTypes = myDTP,
                          Date = myDT,
                          Instrument = list(Properties =  NULL,
                                            Value = "ABC"),
                          Tag = NULL))
  myDRExp[2] <- list(list(DataTypes = myDTP,
                          Date = myDT,
                          Instrument = list(Properties =  NULL,
                                            Value = "DEF"),
                          Tag = NULL))

  myReqExpected <- list(DataRequests = myDRExp,
                        Properties = list(Properties=NULL),
                        TokenValue = "xyz")


  expect_identical(myReq$requestList, myReqExpected)
  expect_equal(myReq$numDatatype, 2L)
  expect_equal(myReq$numInstrument, 2L)

})

