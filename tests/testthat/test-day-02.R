# Part One ------------------------------------------------------

test_that("game_parser() gets the ID and highest cube numbers from a game", {
  game_1 <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  df_1 <- data.frame(
    id = "1",
    red_highest = 4,
    green_highest = 2,
    blue_highest = 6
  )
  expect_equal(game_parser(game_1), df_1)

  game_2 <- "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  df_2 <- data.frame(
    id = "2",
    red_highest = 1,
    green_highest = 3,
    blue_highest = 4
  )
  expect_equal(game_parser(game_2), df_2)

  game_3 <- "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  df_3 <- data.frame(
    id = "3",
    red_highest = 20,
    green_highest = 13,
    blue_highest = 6
  )
  expect_equal(game_parser(game_3), df_3)

  game_4 <- "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  df_4 <- data.frame(
    id = "4",
    red_highest = 14,
    green_highest = 3,
    blue_highest = 15
  )
  expect_equal(game_parser(game_4), df_4)

  game_5 <- "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  df_5 <- data.frame(
    id = "5",
    red_highest = 6,
    green_highest = 3,
    blue_highest = 2
  )
  expect_equal(game_parser(game_5), df_5)
})

test_that("is_game_possible() correctly identifies if a game is possible or not", {
  max_cubes <- data.frame(
    red_max = 12,
    green_max = 13,
    blue_max = 14
  )

  game_1 <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  expect_equal(is_possible_game(game_parser(game_1), max_cubes), TRUE)

  game_2 <- "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  expect_equal(is_possible_game(game_parser(game_2), max_cubes), TRUE)

  game_3 <- "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  expect_equal(is_possible_game(game_parser(game_3), max_cubes), FALSE)

  game_4 <- "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  expect_equal(is_possible_game(game_parser(game_4), max_cubes), FALSE)

  game_5 <- "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  expect_equal(is_possible_game(game_parser(game_5), max_cubes), TRUE)
})

test_that("possible_id_sum() correctly sums the IDs of all possible game", {
  input <- c("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
             "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
             "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
             "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
             "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
             )

  max_cubes <- data.frame(
    red_max = 12,
    green_max = 13,
    blue_max = 14
  )

  expect_equal(possible_id_sum(input, max_cubes), 8)
})

# Part Two ------------------------------------------------------

test_that("power_of_min_cubes() gets the correct power", {
  game_1 <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

  expect_equal(power_of_min_cubes(game_parser(game_1)), 48)
})

test_that("power_of_cubes_sum() returns the power of the min cubes", {
  input <- c("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
             "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
             "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
             "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
             "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )

  expect_equal(power_of_cubes_sum(input), 2286)
})

