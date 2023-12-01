load_test_data <- function(day = "00") {

  input_path <- test_path("test-data", paste0("day-", day, ".txt"))

  readLines(input_path, warn = FALSE)

}

load_real_data <- function(day = "00") {

  input_path <- system.file(
    paste0("day-", day, ".txt"),
    package = "aoc.rstats.template"
  )

  readLines(input_path, warn = FALSE)

}
