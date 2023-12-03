# Part One ------------------------------------------------------

first_digit <- function(string) {
  # Split the string into a character vector
  # with an element for each vector
  characters <- string |>
    strsplit("") |>
    unlist()

  # Get digits from the character vector
  # Specifying the first one with [[1]]
  digit <- grep("[0-9]", characters, value = TRUE)[[1]] |>
    as.numeric()
}

last_digit <- function(string) {
  # Split the string into a character vector
  # with an element for each vector
  characters <- string |>
    strsplit("") |>
    unlist()

  # Get digits from the character vector
  digits <- grep("[0-9]", characters, value = TRUE)

  # Identify the last element with the length
  digit <-  digits[length(digits)] |>
    as.numeric()
}

calibration_value <- function(string) {
  first_digit <- first_digit(string)
  last_digit <- last_digit(string)

  value <- paste0(first_digit, last_digit) |>
    as.numeric()
}

calibration_sum <- function(input) {
  values <- lapply(input, calibration_value) |>
    as.numeric()

  sum <- sum(values)
}

# For solution:
# print(solve_day_01())
solve_day_01_p1 <- function() {
  input <- load_real_data("01")

  print(calibration_sum(input))
}

# Part Two ------------------------------------------------------

valid_numbers <- function() {
  return(c("one" = 1,
           "two" = 2,
           "three" = 3,
           "four" = 4,
           "five" = 5,
           "six" = 6,
           "seven" = 7,
           "eight" = 8,
           "nine" = 9))
}

get_numbers <- function(string) {
  # For every valid number, find its location in the string
  # Going on pairs of text digit equivalents e.g. 1 and "one" first
  number_locations <- lapply(names(valid_numbers()),
                             function(number_name) {
                               num_number <- gregexpr(valid_numbers()[[number_name]], string)
                               chr_number <- gregexpr(number_name, string)
                               return(list(num_number, chr_number))
                             }) |>
    unlist(recursive = FALSE)

  # Extract the start location and length of valid numbers in the text
  starts <- unlist(number_locations)
  lengths <- unlist(lapply(number_locations,
                           function(x) {
                             attr(x[[1]], "match.length")
                           }))

  # Use the starts and lengths to extract all of the numbers
  numbers <- mapply(function(start, length) {
    if (start == -1)
      return(-1)

    number <- substr(string, start, start + length - 1)

    if (number %in% names(valid_numbers())) {
      number = valid_numbers()[[number]]
    }

    return(number)
  },
  starts,
  lengths)

  # Use the starts to order the numbers
  # The start is -1 when the number was not found
  data <- data.frame(starts, numbers) |>
    subset(starts != -1)

  data <- data[order(data$starts),]

  as.numeric(data$numbers)
}

first_number <- function(string) {
  return(get_numbers(string)[[1]])
}

last_number <- function(string) {
  numbers <- get_numbers(string)

  return(numbers[length(numbers)])
}

calibration_value2 <- function(string) {
  as.numeric(paste0(first_number(string), last_number(string)))
}

calibration_sum2 <- function(input) {
  values <- unlist(lapply(input, calibration_value2))

  sum(as.numeric(values))
}

solve_day_01_p2 <- function() {
  input <- load_real_data("01")

  print(calibration_sum2(input))
}

