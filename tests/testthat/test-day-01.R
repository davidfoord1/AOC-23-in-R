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
