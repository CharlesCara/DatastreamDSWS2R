##############################################################################################

context("classConstructor.R : test of initialise")


##############################################################################################
test_that("test of creating a dsws object with no parameters", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()



mydsws <- dsws$new()

expect_is(mydsws, "dsws")

rm(mydsws)

})


##############################################################################################
test_that("test of creating a dsws object with bad username", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()


  expect_error(dsws$new(username = "MickeyMouse", password = "Goofy2"))


})


##############################################################################################
test_that("test of creating a dsws object wrong server location", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

  expect_error(dsws$new(dsws.serverURL = "https://www.bbc.co.uk/",
                        username = "MickeyMouse",
                        password = "Goofy2"))


})

##############################################################################################
test_that("test of creating a dsws object without connecting to dsws server", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()



  mydsws <- dsws$new(connect = FALSE)

  expect_is(mydsws, "dsws")

  rm(mydsws)

})

##############################################################################################
test_that("test of creating a dsws object with callback function for getting Token", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()

    myTokenFunction <- function(){
      list(TokenValue = paste0("abc:", Sys.time()),
           TokenExpiry = Sys.time() + as.difftime(2, units = "mins"))
    }

  mydsws <- dsws$new(getTokenFunction = myTokenFunction)

  expect_is(mydsws, "dsws")
  expect_equal(stringi::stri_sub(mydsws$tokenList$TokenValue, to = 3L) ,"abc")

  mydsws$.loadToken()



  rm(mydsws)

})
