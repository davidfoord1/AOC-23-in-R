# Part One ------------------------------------------------------

test_that("first_digit() gets the first digit", {
  expect_equal(first_digit("1abc2"), 1)
  expect_equal(first_digit("pqr3stu8vwx"), 3)
  expect_equal(first_digit("a1b2c3d4e5f"), 1)
  expect_equal(first_digit("treb7uchet"), 7)
})

test_that("last_digit() gets the last digit", {
  expect_equal(last_digit("1abc2"), 2)
  expect_equal(last_digit("pqr3stu8vwx"), 8)
  expect_equal(last_digit("a1b2c3d4e5f"), 5)
  expect_equal(last_digit("treb7uchet"), 7)
})

test_that("calibration_value() gets the first and last digit", {
  expect_equal(calibration_value("1abc2"), 12)
  expect_equal(calibration_value("pqr3stu8vwx"), 38)
  expect_equal(calibration_value("a1b2c3d4e5f"), 15)
  expect_equal(calibration_value("treb7uchet"), 77)
})

test_that("calibration_sum() gets the sum of all calibration values", {
  input <- c("1abc2",
             "pqr3stu8vwx",
             "a1b2c3d5e5f",
             "treb7uchet")

  expect_equal(calibration_sum(input), 142)
})


# Part Two ------------------------------------------------------

test_that("get_numbers() gets all numbers", {
  expect_equal(get_numbers("two1nine"), c(2, 1, 9))
  expect_equal(get_numbers("eightwothree"), c(8, 2, 3))
  expect_equal(get_numbers("abcone2threexyz"), c(1, 2, 3))
  expect_equal(get_numbers("xtwone3four"), c(2, 1, 3, 4))
  expect_equal(get_numbers("4nineeightseven2"), c(4, 9, 8, 7, 2))
  expect_equal(get_numbers("zoneight234"), c(1, 8, 2, 3, 4))
  expect_equal(get_numbers("7pqrstsixteen"), c(7, 6))
})

test_that("first_number() gets the first numbers", {
  expect_equal(first_number("two1nine"), 2)
  expect_equal(first_number("eightwothree"), 8)
  expect_equal(first_number("abcone2threexyz"), 1)
  expect_equal(first_number("xtwone3four"), 2)
  expect_equal(first_number("4nineeightseven2"), 4)
  expect_equal(first_number("zoneight234"), 1)
  expect_equal(first_number("7pqrstsixteen"), 7)
})

test_that("last_number() gets the last number", {
  expect_equal(last_number("two1nine"), 9)
  expect_equal(last_number("eightwothree"), 3)
  expect_equal(last_number("abcone2threexyz"), 3)
  expect_equal(last_number("xtwone3four"), 4)
  expect_equal(last_number("4nineeightseven2"), 2)
  expect_equal(last_number("zoneight234"), 4)
  expect_equal(last_number("7pqrstsixteen"), 6)
})

test_that("calibration_value2() returns the first and last numbers together", {
  expect_equal(calibration_value2("two1nine"), 29)
  expect_equal(calibration_value2("eightwothree"), 83)
  expect_equal(calibration_value2("abcone2threexyz"), 13)
  expect_equal(calibration_value2("xtwone3four"), 24)
  expect_equal(calibration_value2("4nineeightseven2"), 42)
  expect_equal(calibration_value2("zoneight234"), 14)
  expect_equal(calibration_value2("7pqrstsixteen"), 76)
})

test_that("calibration_sum2 returns the sum of all calibration values", {
  input <- c("two1nine",
             "eightwothree",
             "abcone2threexyz",
             "xtwone3four",
             "4nineeightseven2",
             "zoneight234",
             "7pqrstsixteen")

  expect_equal(calibration_sum2(input), 281)
})

