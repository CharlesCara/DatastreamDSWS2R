##############################################################################################

context("classConstructor.R : test of listRequest method")

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {

  if (Sys.getenv("DatastreamUsername") == "") {
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new(connect = FALSE)

  myTime <- Sys.time()

  tokenTime <- myTime + as.difftime(120, units = "secs")

  myToken <- list(TokenValue = "abc",
                  TokenExpiry = tokenTime)


  expect_false(mydsws$.tokenExpired(thisToken = myToken,
                                    myTime = myTime))
  expect_false(mydsws$.tokenExpired(thisToken = myToken,
                                    myTime = myTime + as.difftime(58, units = "secs")))
  expect_true(mydsws$.tokenExpired(thisToken = myToken,
                                   myTime = myTime + as.difftime(62, units = "secs")))
  expect_true(mydsws$.tokenExpired(thisToken = myToken,
                                   myTime = myTime + as.difftime(121, units = "secs")))
})