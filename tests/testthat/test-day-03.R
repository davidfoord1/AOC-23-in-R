# Part One ------------------------------------------------------

test_that("part_number_sum() returns the correct total of part numbers", {
  input <- load_test_data("03")

  expect_equal(part_number_sum(input), 4361)
})

# Part Two ------------------------------------------------------

test_that("gear_ratio_sum() returns the correct total of gear ratios", {
  input <- load_test_data("03")

  expect_equal(gear_ratio_sum(input), 467835)
})

