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



test_that("One series is zero width", {
  test1 <- structure(c(3.7, 3.68, 3.673, 3.645, 3.645, 3.646, 3.596, 3.598,
                       3.595, 3.586, 3.589, 3.567, 3.568, 3.595, 3.601, 3.579, 3.59,
                       3.558, 3.613, 3.592, 3.597, 3.597, 3.551, 3.49, 3.504, 3.522,
                       3.539, 3.518, 3.532, 3.544, 3.528, 3.555, 3.569, 3.569, 3.527,
                       3.462, 3.537, 3.5, 3.502, 3.466, 3.466, 3.467, 9.6606, 9.6839,
                       9.6531, 9.6747, 9.6709, 9.6671, 9.4922, 9.6376, 9.6204, 9.7545,
                       9.794, 9.6698, 9.666, 9.6234, 9.5329, 9.6141, 9.9029, 9.7984,
                       9.8156, 9.7629, 9.7141, 9.5564, 9.4849, 9.5009, 9.4742, 9.4722,
                       9.4407, 9.4911, 9.7494, 9.7041, 9.6746, 9.7017, 9.8041, 9.781,
                       9.7964, 9.7909, 9.8294, 9.799, 9.6924, 9.6033, 9.5591, 9.5553),
  index = structure(c(1555286400, 1555372800, 1555459200, 1555545600,
                         1555632000, 1555891200, 1555977600, 1556064000, 1556150400, 1556236800,
                         1556496000, 1556582400, 1556668800, 1556755200, 1556841600, 1557100800,
                         1557187200, 1557273600, 1557360000, 1557446400, 1557705600, 1557792000,
                         1557878400, 1557964800, 1558051200, 1558310400, 1558396800, 1558483200,
                         1558569600, 1558656000, 1558915200, 1559001600, 1559088000, 1559174400,
                         1559260800, 1559520000, 1559606400, 1559692800, 1559779200, 1559865600,
                         1560124800, 1560211200), tzone = "UTC", tclass = "Date"),
  class = c("xts", "zoo"),
  .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC",
  .Dim = c(42L, 2L),
  .Dimnames = list(NULL, c("CH0038863350", "GB00B03MLX29")))

  test2 <- structure(logical(0), class = c("xts", "zoo"),
                     .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", index = structure(numeric(0), tzone = "UTC", tclass = "Date"),
                     .Dim = c(0L,  4L),
                     .Dimnames = list(NULL, c("SE0004840718", "SE0009973548", "IS0000020709", "IS0000024602")))

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



