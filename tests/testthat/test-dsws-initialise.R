##############################################################################################

context("classConstructor.R : test of initialise")


##############################################################################################
test_that("test of creating a dsws object with no parameters", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()



mydsws <- dsws$new()

expect_is(mydsws, "dsws")

rm(mydsws)

})


##############################################################################################
test_that("test of creating a dsws object with bad username", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()


  expect_error(dsws$new(username = "MickeyMouse", password = "Goofy2"))


})


##############################################################################################
test_that("test of creating a dsws object wrong server location", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()

  expect_error(dsws$new(dsws.serverURL = "http://www.bbc.co.uk"))


})

##############################################################################################
test_that("test of creating a dsws object without connecting to dsws server", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()



  mydsws <- dsws$new(connect = FALSE)

  expect_is(mydsws, "dsws")

  rm(mydsws)

})

