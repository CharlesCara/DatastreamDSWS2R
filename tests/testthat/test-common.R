context("common.R : test of common functions")
source("~/.RProfile")
suppressMessages(.First())
#

test_that("jsonConvertdatetime is works for simple, single date", {

  jsonDate <- "/Date(1449218172199)/"
  rDate <- DatastreamDSWS2R:::.convert_JSON_Datetime(jsonDate)

  expect_equal(rDate, as.POSIXlt("2015-12-04 08:36:12 GMT"), tolerance = 0.5)

})

test_that("jsonConvertdate is works for simple, single date", {

  jsonDate <- "/Date(1449218172199)/"
  rDate <- DatastreamDSWS2R:::.convert_JSON_Date(jsonDate)

  expect_equal(rDate, as.Date("2015-12-04"), tolerance = 0.5)

})

test_that("jsonConvertdate is works for date array", {

  jsonDate <- c("/Date(1449218172199)/", "/Date(1318896000000+0000)/")
  rDate <- DatastreamDSWS2R:::.convert_JSON_Date(jsonDate)

  expect_equal(rDate,
               c(as.Date("2015-12-04"), as.Date("2011-10-18")),
               tolerance = 0.5)

})


test_that(".convertJSONString is works a variety of combinations", {

  expect_equal(DatastreamDSWS2R:::.convertJSONString("/Date(1318896000000+0000)/"),
               as.Date("2011-10-18"), tolerance = 0.5)

  expect_equal(DatastreamDSWS2R:::.convertJSONString("28270000"),
               28270000, tolerance = 0.5)

  expect_equal(DatastreamDSWS2R:::.convertJSONString("abc"),
               "abc", tolerance = 0.5)

  expect_equal(DatastreamDSWS2R:::.convertJSONString("/Date(1449218172199)/"),
               as.Date("2015-12-04"), tolerance = 0.5)
})


test_that(".getValueTyped is works a variety of combinations", {

  myTest <- list(Value = "/Date(1318896000000+0000)/",
                 Type = 4)

  expect_equal(DatastreamDSWS2R:::.getValueTyped(x = myTest, myType= 4),
               as.Date("2011-10-18"), tolerance = 0.5)

  myTest <- list(Value = "NA",
                 Type = 6)

  expect_equal(DatastreamDSWS2R:::.getValueTyped(x = myTest, myType= 4),
               as.Date(NA), tolerance = 0.5)
})