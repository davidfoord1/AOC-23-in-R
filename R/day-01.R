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

read_input_txt <- function(file_name) {
  data_dir <- file.path("tests", "testthat", "test-data")
  file_path <- file.path(data_dir, file_name)

  scan(file_path, character(), sep = "\n")
}

# For solution:
# print(solve_day_01())
solve_day_01 <- function() {
  input <- read_input_txt("day-01.txt")

  return(calibration_sum(input))
}
