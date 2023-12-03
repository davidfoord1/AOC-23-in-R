# Part One ---------------------------------------------------

#' Return starts and lengths of all numbers in input
identify_numbers <- function(input) {
  numbers <- gregexpr("[0-9]+", input)
}

#' Check each position adjacent to a cell for symbols except "."
symbol_adjacent <- function(data, row, col) {
  for (adj_row in -1:1) {
    pos_row <- row + adj_row
    # Skip row if out of bounds
    if (pos_row < 1 || pos_row > length(data[row, ])) {
      next
    }

    for (adj_col in -1:1) {
      pos_col <- col + adj_col

      # Skip col if out of bounds
      if (pos_col < 1 || pos_col > length(data)) {
        next
      }

      character <- data[row + adj_row, col + adj_col]

      # Regex check for any symbol except "."
      if(grepl("(?![.])[[:punct:]]", character, perl = TRUE)) {
        return(TRUE)
      }
    }
  }

  # If no symbols found in any adjacent cell.-
  return(FALSE)
}

is_part_number <- function(data, start, length, line) {
  # From the start, up to the length
  # Check all adjacent squares for symbols
  for (position in start:(start + length - 1)) {
    if (symbol_adjacent(data, position, line)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

part_numbers_in_line <- function(input, data, number_locations, line) {
  part_numbers <- NA_integer_

  # Loop through each number in the line
  for(number in seq_along(number_locations[[line]])) {
    # Find the start position
    start <- number_locations[[line]][[number]]

    # Find the length
    length <- attr(number_locations[[line]], "match.length")[[number]]

    if(!is_part_number(data, start, length, line)) {
      next #number in the line
    }

    part_numbers <- part_numbers |>
      c(substr(input[[line]],
               start,
               start + length - 1
      ))
  }

  return(part_numbers)
}

part_number_sum <- function(input) {
  # Get starts and lengths
  number_locations <- identify_numbers(input)

  # Each line becomes a column in a data frame (not a row!)
  data <- input |>
    strsplit("") |>
    as.data.frame()

  # Intialise for storing parts
  part_numbers <- NA_integer_

  # Loop through each line
  for (line in seq_along(number_locations)) {
    # Go to the next line if there are no valid starts
    if (number_locations[[line]][[1]] == -1) {
      next # line
    }

    # If there are numbers in the line
    # Check if they are valid part numbers
    # And append to the full part vector
    part_numbers <- part_numbers |>
      c(part_numbers_in_line(input, data, number_locations, line))
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
  gears <- gregexpr("\\*", input)
}

adjacent_number_locations <- function(data, row, col) {
  numbers <- list()

  for (adj_row in -1:1) {
    pos_row <- row + adj_row
    # Skip row if out of bounds
    if (pos_row < 1 || pos_row > length(data[row, ])) {
      next
    }

    for (adj_col in -1:1) {
      pos_col <- col + adj_col

      # Skip col if out of bounds
      if (pos_col < 1 || pos_col > length(data)) {
        next
      }

      character <- data[pos_row, pos_col]
      if(grepl("[[:digit:]]", character, perl = TRUE)) {
        numbers <- c(numbers, list(c(pos_row, pos_col)))
      }
    }
  }

  return(numbers)
}

part_numbers_from_digits <- function(input, number_locations, digit_locs) {
  part_numbers <- NA_integer_

  # digit_locs is a list
  # digit_loc structure
  # [[1]] position in line
  # [[2]] line number
  for(digit_loc in digit_locs) {
    number_line <- digit_loc[[2]]

    for(number in seq_along(number_locations[[number_line]])) {
      # Find the start position
      start <- number_locations[[number_line]][[number]]

      # Exit the number loop if there are no valid starts
      if (start == -1) break

      # Find the length
      length <- attr(number_locations[[number_line]], "match.length")[[number]]

      range <- start:(start + length - 1)

      if(digit_loc[[1]] %in% range) {
        part_numbers <- part_numbers |>
          c(substr(input[[number_line]],
                   start,
                   start + length - 1
          ))
      }
    }
  }

  # Remove duplicates
  # Because multiple digits may be adjacent to the gear
  # The parts are unique
  part_numbers <- unique(part_numbers) |>
    na.omit() |>
    as.numeric()

  return(part_numbers)
}

gear_ratios_from_line <- function(input, data, gear_locations, number_locations, line) {
  gear_ratios <- NA_integer_

  for(gear in seq_along(gear_locations[[line]])) {
    gear_numbers <- NA_integer_

    # Find the gear position
    position <- gear_locations[[line]][[gear]]

    # Exit the number loop if there are no valid starts
    if (position == -1) break

    adj_num_locs <- adjacent_number_locations(data,
                                              position,
                                              line)

    # Append numbers to vector where a digit is adjacent to the gear
    gear_numbers <- gear_numbers |>
      c(part_numbers_from_digits(input, number_locations, adj_num_locs))

    gear_numbers <- gear_numbers |>
      na.omit()

    if(length(gear_numbers) == 2) {
      gear_ratios <- gear_ratios |>
        c(
          gear_numbers[[1]] * gear_numbers[[2]]
        )
    }
  }

  return(gear_ratios)
}

gear_ratio_sum <- function(input) {
  # Get starts and lengths
  gear_locations <- identify_gears(input)
  number_locations <- identify_numbers(input)

  # Each line becomes a column in a data frame (not a row!)
  data <- input |>
    strsplit("") |>
    as.data.frame()

  # Intialise for storing parts
  gear_ratios <- NA_integer_

  # Loop through each line
  for(line in seq_along(gear_locations)) {
    gear_ratios <- gear_ratios |>
      c(gear_ratios_from_line(input,
                              data,
                              gear_locations,
                              number_locations,
                              line))
  }

  gear_ratios <- gear_ratios |>
    na.omit() # Remove NA used to initialise

  return(sum(gear_ratios))
}

solve_day_03_p2 <- function() {
  input <- load_real_data("03")

  print(gear_ratio_sum(input))
}

