##############################################################################################

context("classConstructor.R : test of initialise")
source("~/.RProfile")
suppressMessages(.First())


##############################################################################################
test_that("test of creating a dsws object with no parameters", {


mydsws <- dsws$new()

expect_is(mydsws, "dsws")

rm(mydsws)

})


##############################################################################################
test_that("test of creating a dsws object with bad username", {


  mydsws <- dsws$new(username = "MickeyMouse")

  expect_null(mydsws$tokenList$TokenValue, "dsws")

  rm(mydsws)

})


##############################################################################################
test_that("test of creating a dsws object wrong server location", {

  expect_error(dsws$new(dsws.serverURL = "http://www.bbc.co.uk"))


})

##############################################################################################
test_that("test of creating a dsws object without connecting to dsws server", {


  mydsws <- dsws$new(connect = FALSE)

  expect_is(mydsws, "dsws")

  rm(mydsws)

})

