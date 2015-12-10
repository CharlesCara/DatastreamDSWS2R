##############################################################################################

context("classConstructor.R : test of listRequest method")
source("~/.RProfile")
suppressMessages(.First())

##############################################################################################

test_that("test of simple snapshot request for price datatype with relative dates", {



  mydsws <- dsws$new()

  myData <- mydsws$listRequest(instrument = "LFTSE100",
                                   datatype = "P",
                                   requestDate = "0D")

  expect_is(myData, "data.frame")
  expect_is(myData[1,2], "numeric")
  expect_more_than(nrow(myData), 99)
  expect_equal(ncol(myData), 2)

})

