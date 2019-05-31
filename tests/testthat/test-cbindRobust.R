context("Testing cbindRobust")


test_that("1 Character, 1 NA series: Single row, same date", {
  test1 <- xts::xts(matrix(c("a","b"), ncol = 2),
              order.by = as.Date("2019-03-19"))
  test2 <- xts::xts(matrix(NA, ncol = 2),
              order.by = as.Date("2019-03-19"))


  test <- cbindRobust(xts1 = test1, xts2 = test2)

  expectedResult <- xts::xts(matrix(c("a","b",NA,NA), ncol = 4, nrow = 1),
                        order.by = as.Date("2019-03-19"))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Single row, different dates", {
  test1 <- xts::xts(matrix(c("a","b"), ncol = 2),
              order.by = as.Date("2019-03-19"))
  test2 <- xts::xts(matrix(NA, ncol = 2),
              order.by = as.Date("2019-03-20"))

  test <- cbindRobust(xts1 = test1, xts2 = test2)

  expectedResult <- xts::xts(matrix(c("a",NA,"b",NA,NA,NA,NA,NA), ncol = 4, nrow = 2),
                        order.by = as.Date(c("2019-03-19", "2019-03-20")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Multiple but same number of rows, same dates", {
  test1 <- xts::xts(matrix(c("a", "b", "c", "d", "e", "f"), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  test2 <- xts::xts(matrix(NA, ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))

  test <- cbindRobust(xts1 = test1, xts2 = test2)

  expectedResult <- xts::xts(matrix(c("a","b","c","d","e","f",NA,NA,NA,NA,NA,NA), ncol = 4, nrow = 3),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Multiple but same number of rows, different dates", {
  test1 <- xts::xts(matrix(c("a","b","c","d","e","f"), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  test2 <- xts::xts(matrix(NA, ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-22","2019-03-23","2019-03-24")))

  test <- cbindRobust(xts1 = test1, xts2 = test2)

  expectedResult <- xts::xts(matrix(c("a","b","c",NA,NA,NA,
                                 "d","e","f",NA,NA,NA,
                                 NA,NA,NA,NA,NA,NA,
                                 NA,NA,NA,NA,NA,NA), ncol = 4, nrow = 6),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21","2019-03-22","2019-03-23","2019-03-24")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("1 Character, 1 NA series: Multiple but different number of rows, different dates", {
  test1 <- xts::xts(matrix(c("a","b","c","d","e","f"), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  test2 <- xts::xts(matrix(NA, ncol = 2, nrow = 4),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21","2019-03-22")))

  test <- cbindRobust(xts1 = test1, xts2 = test2)


  expectedResult <- xts::xts(matrix(c("a","b","c",NA,
                                 "d","e","f",NA,
                                 NA,NA,NA,NA,
                                 NA,NA,NA,NA), ncol = 4, nrow = 4),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21","2019-03-22")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("One Numeric, one NA series", {
  test1 <- xts::xts(matrix(c(1:6), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  test2 <- xts::xts(matrix(NA, ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))

  test <- cbindRobust(xts1 = test1, xts2 = test2)

  expectedResult <- xts::xts(matrix(c(1,2,3,4,5,6,NA,NA,NA,NA,NA,NA), ncol = 4, nrow = 3),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("Both series numeric", {
  test1 <- xts::xts(matrix(c(1:6), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))
  test2 <- xts::xts(matrix(c(7:12), ncol = 2, nrow = 3),
              order.by = as.Date(c("2019-03-19","2019-03-20","2019-03-21")))

  test <- cbindRobust(xts1 = test1, xts2 = test2)

  expectedResult <- xts::xts(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), ncol = 4, nrow = 3),
                        order.by = as.Date(c("2019-03-19", "2019-03-20", "2019-03-21")))

  expect_true(xts::is.xts(test))
  expect_equivalent(test, expectedResult)

})


test_that("One series is zero width", {
  test1 <- structure(c(0.043, 0.043, 0.043, 0.092, 0.0924, 0.0926, 0.063,
                      0.063, 0.062, 0.068, 0.068, 0.068, 0.0402, 0.0402, 0.04), class = c("xts",
                                                                                          "zoo"), .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC", index = structure(c(1553558400,
                                                                                                                                                                                              1553644800, 1553731200), tzone = "UTC", tclass = "Date"), .Dim = c(3L,
                                                                                                                                                                                                                                                                 5L), .Dimnames = list(NULL, c("CH0038863350.EPS1TR12..CH0038863350",
                                                                                                                                                                                                                                                                                               "GB00B03MLX29.EPS1TR12..GB00B03MLX29", "CH0012005267.EPS1TR12..CH0012005267",
                                                                                                                                                                                                                                                                                               "CH0012032048.EPS1TR12..CH0012032048", "FR0000121014.EPS1TR12..FR0000121014"
                                                                                                                                                                                                                                                                 )))
  test2 <- structure(logical(0), class = c("xts", "zoo"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", index = structure(numeric(0), tzone = "UTC", tclass = "Date"), .Dim = 0:1, .Dimnames = list(
    NULL, "IS0000000503"))

  test <- expect_silent(cbindRobust(xts1 = test1, xts2 = test2))


  expect_true(xts::is.xts(test))
  expect_true(ncol(test) == 6)

})



test_that("Two series are zero width", {
  test1 <-structure(logical(0), class = c("xts", "zoo"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", index = structure(numeric(0), tzone = "UTC", tclass = "Date"), .Dim = 0:1, .Dimnames = list(
    NULL, "Stock1"))


  test2 <- structure(logical(0), class = c("xts", "zoo"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", index = structure(numeric(0), tzone = "UTC", tclass = "Date"), .Dim = 0:1, .Dimnames = list(
    NULL, "Stock2"))

  test <- expect_silent(cbindRobust(xts1 = test1, xts2 = test2))


  expect_true(xts::is.xts(test))
  expect_true(ncol(test) == 2)

})



