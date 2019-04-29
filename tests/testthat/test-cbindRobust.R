context("Testing cbindRobust")


test_that("1 Character, 1 NA series: Single row, same date", {
  var1 <- xts::xts(matrix(c("a","b"), ncol = 2),
              order.by = as.Date("2019-03-19"))
  var2 <- xts::xts(matrix(NA, ncol = 2),
              order.by = as.Date("2019-03-19"))


  test <- cbindRobust(xts1 = var1, xts2 = var2)

  expectedResult <- xts::xts(matrix(c("a","b",NA,NA), ncol = 4, nrow = 1),
                        order.by = as.Date("2019-03-19"))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Single row, different dates", {
  var1 <- xts::xts(matrix(c("a","b"), ncol = 2),
              order.by = as.Date("2019-03-19"))
  var2 <- xts::xts(matrix(NA, ncol = 2),
              order.by = as.Date("2019-03-20"))

  test <- cbindRobust(xts1 = var1, xts2 = var2)

  expectedResult <- xts::xts(matrix(c("a",NA,"b",NA,NA,NA,NA,NA), ncol = 4, nrow = 2),
                        order.by = as.Date(c("2019-03-19", "2019-03-20")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Multiple but same number of rows, same dates", {
  var1 <- xts::xts(matrix(c("a", "b", "c", "d", "e", "f"), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  var2 <- xts::xts(matrix(NA, ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))

  test <- cbindRobust(xts1 = var1, xts2 = var2)

  expectedResult <- xts::xts(matrix(c("a","b","c","d","e","f",NA,NA,NA,NA,NA,NA), ncol = 4, nrow = 3),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Multiple but same number of rows, different dates", {
  var1 <- xts::xts(matrix(c("a","b","c","d","e","f"), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  var2 <- xts::xts(matrix(NA, ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-22","2019-03-23","2019-03-24")))

  test <- cbindRobust(xts1 = var1, xts2 = var2)

  expectedResult <- xts::xts(matrix(c("a","b","c",NA,NA,NA,
                                 "d","e","f",NA,NA,NA,
                                 NA,NA,NA,NA,NA,NA,
                                 NA,NA,NA,NA,NA,NA), ncol = 4, nrow = 6),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21","2019-03-22","2019-03-23","2019-03-24")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Multiple but different number of rows, different dates", {
  var1 <- xts::xts(matrix(c("a","b","c","d","e","f"), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  var2 <- xts::xts(matrix(NA, ncol = 2, nrow = 4),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21","2019-03-22")))

  test <- cbindRobust(xts1 = var1, xts2 = var2)


  expectedResult <- xts::xts(matrix(c("a","b","c",NA,
                                 "d","e","f",NA,
                                 NA,NA,NA,NA,
                                 NA,NA,NA,NA), ncol = 4, nrow = 4),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21","2019-03-22")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("One Numeric, one NA series", {
  var1 <- xts::xts(matrix(c(1:6), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  var2 <- xts::xts(matrix(NA, ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))

  test <- cbindRobust(xts1 = var1, xts2 = var2)

  expectedResult <- xts::xts(matrix(c(1,2,3,4,5,6,NA,NA,NA,NA,NA,NA), ncol = 4, nrow = 3),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("Both series numeric", {
  var1 <- xts::xts(matrix(c(1:6), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  var2 <- xts::xts(matrix(c(7:12), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))

  test <- cbindRobust(xts1 = var1, xts2 = var2)

  expectedResult <- xts::xts(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), ncol = 4, nrow = 3),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


