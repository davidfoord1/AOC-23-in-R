# Part One ------------------------------------------------------

str_extract_numbers <- function(string, pattern = "[[:digit:]]+") {
  numbers <- string |>
    regmatches(m = gregexpr(pattern,
                            string,
                            perl = TRUE)) |>

    unlist() |>
    as.numeric()
}

winning_numbers <- function(card) {
  # Split on ":" or "|"
  card <-  unlist(strsplit(card, split = ":|\\|"))

  # From ":" to "|" are winning
  winning <- str_extract_numbers(card[[2]])
  # After "|" are playing
  playing <- str_extract_numbers(card[[3]])

  winners <- playing |>
    lapply(function(number) {
      if (number %in% winning) {
        return(number)
      }

      return(NA)
    }) |>
    unlist() |>
    na.omit() |>
    as.numeric()
}

card_score <- function(winning_numbers) {
  count <- length(winning_numbers)

  if (count < 1) return(0)

  score <- 1 * (2 ^ (count - 1))
}

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

card_pile_count <- function(cards) {
  card_counts <- cards |>
    lapply(\(x) 1) |>
    as.numeric()

  for (card in seq_along(cards)) {
    bonus_card_count <- cards[[card]] |>
      winning_numbers() |>
      bonus_card_count()

    if (bonus_card_count < 1) next

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
