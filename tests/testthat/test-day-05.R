# Part One ------------------------------------------------------

test_that("almanac_seeds() gets the seeds from an almanac", {
  input <- load_test_data("05")

  expect_equal(almanac_seeds(input), c(79, 14, 55, 13))
})

# Part Two ------------------------------------------------------

test_that("smallest_seed_location2() find the smallest location from a range", {
  input <- load_test_data("05")

  expect_equal(smallest_seed_location2(input), 46)
})

