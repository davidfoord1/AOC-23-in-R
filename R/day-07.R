# Part One and Two ------------------------------------------------------

# Part One
# total_hand_winnings(input)
# Part Two
# total_hand_winnings(input, handle_jokers = TRUE)

card_counts <- function(cards, hand, handle_jokers = FALSE) {
  counter <- cards |>
    lapply(
      function(card) {
        locs <- gregexpr(pattern = card, text = hand, fixed = TRUE)
        counts <- locs |> unlist()
        counts <- sum(counts > 0)
      })

  add_joker <- 0

  # Remove and store the joker value so it can be added to the highest count
  if (handle_jokers) {
      joker <- length(cards)
      add_joker = counter[[joker]]
      counter[[joker]] = 0
  }

  # Sort the counts highest to lowest
  counter <- counter |> as.numeric() |> sort(decreasing = TRUE)

  # Add the joker value to the highest count
  counter[[1]] = counter[[1]] + add_joker

  counter
}

first_higher_than_second <- function(hand_1, hand_2, handle_jokers = FALSE) {
  if (handle_jokers) {
    cards <- factor(c("A", "K", "Q", "T", 9:2, "J"))
  } else {
    cards <- factor(c("A", "K", "Q", "J", "T", 9:2))
  }

  counts_1 <- card_counts(cards, hand_1, handle_jokers)
  counts_2 <- card_counts(cards, hand_2, handle_jokers)

  # Look at the highest counts
  # If one is higher than the other, return based on that
  # This handles all hand types excpet Full house vs Three of a kind
  if (counts_1[[1]] > counts_2[[1]]) {
    return(TRUE)
  }

  if (counts_2[[1]] > counts_1[[1]]) {
    return(FALSE)
  }

  # Sort by second highest count
  # For Full house > Three of a kind
  if (counts_1[[2]] > counts_2[[2]]) {
    return(TRUE)
  }

  if (counts_2[[2]] > counts_1[[2]]) {
    return(FALSE)
  }


  # If sorting couldn't be achieved based on hand type
  # Sort by each individual card
  split_hand_1 <- strsplit(hand_1, "") |> unlist()
  split_hand_2 <- strsplit(hand_2, "") |> unlist()

  for (card_num in 1:5) {
    # Get the two cards to compare
    card_1 <- split_hand_1[[card_num]]
    card_2 <- split_hand_2[[card_num]]

    # Try the next card if they are the same
    if(card_1 == card_2) next

    # Sort by the cards factor
    card_comparison <- factor(c(card_1, card_2), cards)
    card_comparison <- sort(card_comparison)

    # Only switch the cards if the order has changed
    return(card_comparison[[1]] == card_1)
  }
}

total_hand_winnings <- function(input, handle_jokers = FALSE) {
  hands <- lapply(input, \(x) strsplit(x, " ") |> unlist())

  # Bubble Sort
  for (unsorted_length in length(hands):2) {
    print(paste("unsorted:", unsorted_length))

    for (index in 1:(unsorted_length - 1)) {
      # Check if two hands should be switched
      # Where [[1]] is the hand ([[2]] is the bid)
      if (first_higher_than_second(hands[[index]][[1]],
                                   hands[[index + 1]][[1]],
                                   handle_jokers)) {
        # Switch using a temporary variable
        temp_hand <- hands[[index]]
        hands[[index]] <- hands[[index + 1]]
        hands[[index + 1]] <- temp_hand
      }
    }
  }

  total_winnings <- 0

  # Multiply the bids by their position in the list
  for (hand_no in seq_along(hands)){
    total_winnings <- total_winnings + (hand_no * as.numeric(hands[[hand_no]][[2]]))
  }

  print(total_winnings)
}
