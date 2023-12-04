# Part One ------------------------------------------------------

test_that("winning_numbers() gets the numbers that won from a card", {
  cards <- load_test_data("04")

  expect_equal(winning_numbers(cards[[1]]), c(83, 86, 17, 48))
  expect_equal(winning_numbers(cards[[2]]), c(61, 32))
  expect_equal(winning_numbers(cards[[3]]), c(21, 1))
  expect_equal(winning_numbers(cards[[4]]), c(84))
  expect_equal(length(winning_numbers(cards[[5]])), 0)
  expect_equal(length(winning_numbers(cards[[6]])), 0)
})

test_that("card_score() gets the correct score from winning numbers", {
  numbers <- load_test_data("04") |>
    lapply(winning_numbers)

  expect_equal(card_score(numbers[[1]]), 8)
  expect_equal(card_score(numbers[[2]]), 2)
  expect_equal(card_score(numbers[[3]]), 2)
  expect_equal(card_score(numbers[[4]]), 1)
  expect_equal(card_score(numbers[[5]]), 0)
  expect_equal(card_score(numbers[[6]]), 0)
})

test_that("card_pile_score() gets the correct total score", {
  cards <- load_test_data("04")

  expect_equal(card_pile_score(cards), 13)
})

# Part Two ------------------------------------------------------

test_that("bonus_card_count() gets the number of cards won by a card", {
  numbers <- load_test_data("04") |>
    lapply(winning_numbers)

  expect_equal(bonus_card_count(numbers[[1]]), 4)
  expect_equal(bonus_card_count(numbers[[2]]), 2)
  expect_equal(bonus_card_count(numbers[[3]]), 2)
  expect_equal(bonus_card_count(numbers[[4]]), 1)
  expect_equal(bonus_card_count(numbers[[5]]), 0)
  expect_equal(bonus_card_count(numbers[[6]]), 0)
})

test_that("card_pile_count() gets the correct card count", {
  cards <- load_test_data("04")

  expect_equal(card_pile_count(cards), 30)
})
