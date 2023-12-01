test_that("day 00 function achieves expected result", {

  input <- load_test_data("00")  # function in R/utils.R

  result <- solve_day_00(input)  # call our function
  expected_result <- 24000  # known result for test data provided by AOC site

  expect_equal(result, expected_result)  # will pass silently

})
