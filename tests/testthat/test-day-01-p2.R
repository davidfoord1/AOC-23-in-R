test_that("get_numbers() gets all numbers", {
  expect_equal(get_numbers("two1nine"), c(2, 1, 9))
  expect_equal(get_numbers("eightwothree"), c(8, 2, 3))
  expect_equal(get_numbers("abcone2threexyz"), c(1, 2, 3))
  expect_equal(get_numbers("xtwone3four"), c(2, 1, 3, 4))
  expect_equal(get_numbers("4nineeightseven2"), c(4, 9, 8, 7, 2))
  expect_equal(get_numbers("zoneight234"), c(1, 8, 2, 3, 4))
  expect_equal(get_numbers("7pqrstsixteen"), c(7, 6))
})
