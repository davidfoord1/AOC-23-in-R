# Part One ------------------------------------------------------

#' Pull groups of numbers from a string
str_extract_numbers <- function(string, pattern = "[[:digit:]]+") {
  numbers <- string |>
    regmatches(m = gregexpr(pattern,
                            string,
                            perl = TRUE)) |>

    unlist() |>
    as.numeric()
}

#' Find all numbers played in a card that are winning
winning_numbers <- function(card) {
  # Split on ":" or "|"
  card <-  unlist(strsplit(card, split = ":|\\|"))

  # From ":" to "|" are numbers that will win
  winners <- str_extract_numbers(card[[2]])
  # After "|" are playing
  playing <- str_extract_numbers(card[[3]])

  winning <- playing |>
    lapply(function(number) {
      if (number %in% winners) {
        return(number)
      }

      return(NA)
    }) |>
    unlist() |>
    na.omit() |>
    as.numeric()
}

#' Score is 2 ^ n - 1, where n is the count of winning numbers
card_score <- function(winning_numbers) {
  count <- length(winning_numbers)

  if (count < 1) return(0)

  score <- 2 ^ (count - 1)
}

#' Get and sum the scores for each card
card_pile_score <- function(cards) {
  scores <- cards |>
    lapply(function(card) {
      score <- card |>
        winning_numbers() |>
        card_score()
    }) |>
    as.numeric()

  return(sum(scores))
}

solve_day_04_p1 <- function() {
  cards <- load_real_data("04")

  print(card_pile_score(cards))
}

# Part Two ------------------------------------------------------

bonus_card_count <- function(winning_numbers) {
  return(length(winning_numbers))
}

#' Get and sum the counts for each card
card_pile_count <- function(cards) {
  # Create a vector of 1s for each card
  card_counts <- cards |>
    lapply(\(x) 1) |>
    as.numeric()

  # For each card (number)
  for (card in seq_along(cards)) {
    # Find the number of following cards that will get a bonus card
    bonus_card_count <- cards[[card]] |>
      winning_numbers() |>
      bonus_card_count()

    # Go the the next card if there are no bonuses
    if (bonus_card_count < 1) next

    # For each of the cards following the current card
    # Which you are due to get a bonus copy of
    # Add the count of the current card to that card's count
    for (bonus in 1:bonus_card_count) {
      count <- card + bonus
      card_counts[[count]] <- card_counts[[count]] + card_counts[[card]]
    }
  }

  return(sum(card_counts))
}

solve_day_04_p2 <- function() {
  cards <- load_real_data("04")

  print(card_pile_count(cards))
}
