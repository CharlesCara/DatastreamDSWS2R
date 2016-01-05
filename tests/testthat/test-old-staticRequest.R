##############################################################################################

context("classConstructor.R : test of staticRequest function")

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {
  if(is.null(options()$Datastream.Username)){
    skip("Username not available")
  }
  skip_on_cran()



  dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)

  myData <- staticRequest(dwei = dwei,
                          DSCode = c("ABF","RIO","WPP"),
                          Expression = "P",
                          endDate = Sys.Date(),
                          verbose = TRUE)

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_equal(nrow(myData), 3)
  expect_equal(ncol(myData), 2)

})




