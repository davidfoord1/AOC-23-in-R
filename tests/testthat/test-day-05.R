# Part One ------------------------------------------------------

test_that("almanac_seeds() gets the seeds from an almanac", {
  input <- load_test_data("05")

  expect_equal(almanac_seeds(input), c(79, 14, 55, 13))
})

test_that("map_source_to_destination() returns a full map of source -> destination", {
  input <- load_test_data("05")

  expect_equals(map_source_to_destionat(input, "seed-to-soil"), )
})


