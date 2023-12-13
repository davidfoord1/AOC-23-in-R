pattern_matrices <- function(input) {
  pattern_seps <- c(0, grep("^$", input), length(input) + 1)

  # Pre allocate
  patterns <- vector("list", length(pattern_seps) - 1)

  for (index in 1:(length(pattern_seps) - 1)) {
    start <- pattern_seps[[index]] + 1
    end <- pattern_seps[[index + 1]] - 1
    patterns[[index]] <- input[start:end]
  }

  for (pat_index in seq_along(patterns)) {
    split_pattern <- strsplit(patterns[[pat_index]], split = "") |> unlist()

    patterns[[pat_index]] <- matrix(split_pattern,
                                    nrow = nchar(patterns[[pat_index]][[1]]),
                                    ncol = length(patterns[[pat_index]])) |>
      t()
  }

  patterns
}


vecs_are_equal <- function(vec1, vec2) {
  return(prod(vec1 == vec2) == 1)
}

col_is_mirror <- function(pattern, index) {
  # Assuming immediately adjacent cols have already been checked
  if (index == 1) return(TRUE)

  sep = 3
  for (col1 in (index - 1):1) {
    col2 <- col1 + sep
    sep = sep + 2
    print(paste("col1", col1, "col2", col2))

    # If all are equal up to the right edge, there is a mirror
    if (col2 > ncol(pattern)) return(TRUE)

    # If any non-matching pairs are found, there is no mirror
    if (!vecs_are_equal(pattern[, col1], pattern[, col2])) {
      print("vecs not equal")
      return(FALSE)
    }

    # If all are equal up to the left edge, there is a mirror
    if (col1 == 1) return(TRUE)
  }
}

row_is_mirror <- function(pattern, index) {
  if (index == 1) return(TRUE)
  # Assuming immediately adjacent cols have already been checked
  sep = 3
  for (row1 in (index - 1):1) {
    row2 <- row1 + sep
    sep <- sep + 2

    # If all are equal up to the right edge, there is a mirror
    if (row2 > nrow(pattern)) return(TRUE)

    # If any non-matching pairs are found, there is no mirror
    if (!vecs_are_equal(pattern[row1, ], pattern[row2, ])) {
      return(FALSE)
    }

    # If all are equal up to the left edge, there is a mirror
    if (row1 == 1) return(TRUE)
  }
}

control_fun <- function(input) {
  patterns <- pattern_matrices(input)

  # Pre allocate count vectors
  cols_left_of_mirror <- numeric(length(patterns))
  rows_above_mirror <- numeric(length(patterns))

  # For Pattern in patterns
  # For column in patterns, check if col and col + 1 are equal
  # Potential mirror if they are equal
  # From a potential mirror iterate col1 -1 and col2 + 1, checking for equality
  # If all are equal until the edge of the grid has been reached, a match has been found

  # Always taking our current index as to the left of the mirror
  # and index + 1 is to teh right of it
  for (index in seq_along(patterns)) {
    for (col in 1:(ncol(patterns[[index]]) - 1)) {
      # Check for potential mirrors by comparing adjacent columns
        if (vecs_are_equal(patterns[[index]][,col], patterns[[index]][,col+1])) {
          # Confirm mirrors by comparing outward from adjacent columns
          if (col_is_mirror(patterns[[index]], col)) {
            cols_left_of_mirror[[index]] <- col
          }
        }
    }

    for (row in 1:(nrow(patterns[[index]]) - 1)) {
      # Check for potential mirrors by comparing adjacent columns
      if (vecs_are_equal(patterns[[index]][row,], patterns[[index]][row+1, ])) {
        # Confirm mirrors by comparing outward from adjacent columns
        if (row_is_mirror(patterns[[index]], row)) {
          rows_above_mirror[[index]] <- row
        }
      }
    }
  }

  # 32866 too low
  p1_result <- sum(cols_left_of_mirror,
                   (rows_above_mirror * 100))
}
