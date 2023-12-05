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

#' Function composition operator
#' lhs, rhs are left, right hand-side
`%.%` <- function(lhs, rhs) {
  # Assign a composed function to a name if lhs is a character string
  if (is.character(lhs)) {
    if (!is.function(rhs)) {
      stop("When lhs is a character string, rhs must be a function.")
    }
    assign(lhs, rhs, pos = .GlobalEnv)
    return(invisible(NULL))
  }

  # Apply lhs to rhs if rhs is not a function
  if (!is.function(rhs)) {
    return(lhs(rhs))
  }

  # Compose lhs and rhs if both are functions
  if (is.function(lhs) && is.function(rhs)) {
    return(function(...) lhs(rhs(...)))
  }

  stop("Invalid input types for %.% operator.")
}


#' Apply a function over a list/vector and, return a numeric vector
map_num <- function(.x, .f) {
  as.numeric %.% lapply(.x, .f)
}
