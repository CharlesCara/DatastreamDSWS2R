##############################################################################################

context("classConstructor.R : test of listRequest method")

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {

  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  mydsws <- dsws$new()

  myTime <- Sys.time()

  tokenTime <- myTime + 2 * 3600

  myToken <- list(TokenValue = "abc",
                  TokenExpiry = tokenTime)


  expect_false(mydsws$.tokenExpired(thisToken = myToken, myTime = myTime))
  expect_false(mydsws$.tokenExpired(thisToken = myToken, myTime = myTime + 3599))
  expect_true(mydsws$.tokenExpired(thisToken = myToken, myTime = myTime + 3701))
  expect_true(mydsws$.tokenExpired(thisToken = myToken, myTime = myTime + 7201))
})