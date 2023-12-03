# Part One ---------------------------------------------------

#' Return a numeric vector of numbers in a line
identify_numbers <- function(input) {
  numbers <- gregexpr("[0-9]+", input, perl = TRUE)
}

#' Check each position adjacent to a cell for symbols except "."
symbol_adjacent <- function(data, row, col) {
  for (adj_row in -1:1) {
    # Skip row if out of bounds
    if (row + adj_row < 1 || row + adj_row > length(data[row, ])) {
      next
    }

    for (adj_col in -1:1) {
      # Skip col if out of bounds
      if (col + adj_col < 1 || col + adj_col > length(data)) {
        next
      }

      character <- data[row + adj_row, col + adj_col]
      if(grepl("(?![.])[[:punct:]]", character, perl = TRUE)) {
       return(TRUE)
      }
    }
  }
  return(FALSE)
}

#' Return any numbers that have adjacent symbols
part_number_sum <- function(input) {
  # Each line becomes a column in a data frame (not a row!)
  data <- input |>
    strsplit("") |>
    as.data.frame()

  all_numbers <- identify_numbers(input)

  part_numbers <- NA_integer_

  # Loop through each line
  for (line in seq_along(all_numbers)) {
    # Loop through each number in the line
    for(number in seq_along(all_numbers[[line]])) {
      # Find the start position
      start <- all_numbers[[line]][[number]]

      # Exit the number loop if there are no valid starts
      if (start == -1) break

      # Find the length
      length <- attr(all_numbers[[line]], "match.length")[[number]]

      # From the start, up to the length
      # Check all adjacent squares for symbols
      for (position in start:(start + length - 1)) {
        if (symbol_adjacent(data, position, line)) {
          part_numbers <- part_numbers |>
            c(substr(input[[line]],
                     start,
                     start + length - 1
            ))

          break
        }
      }

      # If no adjacent symbol found, return 0?
    }
  }

  part_numbers <- part_numbers |>
    na.omit() |>
    as.numeric()

  return(sum(part_numbers))
}

solve_day_03_p1 <- function() {
  input <- load_real_data("03")

  print(part_number_sum(input))
}

# Part Two ------------------------------------------------------

identify_gears <- function(input) {
  numbers <- gregexpr("*", input)
}

gear_ratio_sum <- function(input) {

}

