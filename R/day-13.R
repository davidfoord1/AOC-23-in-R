# solve_day_13(input)

solve_day_13 <- function(input) {
  patterns <- pattern_matrices(input)

  # Pre allocate count vectors
  cols_left_of_mirror <- numeric(length(patterns))
  rows_above_mirror <- numeric(length(patterns))

  cols_left_of_smudged_mirror <- numeric(length(patterns))
  rows_above_smudged_mirror <- numeric(length(patterns))

  # For pattern in patterns
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

        if (col_is_smudged_mirror(patterns[[index]], col, 0)) {
          cols_left_of_smudged_mirror[[index]] <- col
        }
      }

      if (vecs_have_one_difference(patterns[[index]][,col], patterns[[index]][,col+1])) {
        if (col_is_smudged_mirror(patterns[[index]], col, 1)) {
          cols_left_of_smudged_mirror[[index]] <- col
        }
      }
    }

    # Repeat the same for rows
    # Yes, I copy-pasted and edited instead of functionalising

    for (row in 1:(nrow(patterns[[index]]) - 1)) {
      # Check for potential mirrors by comparing adjacent columns
      if (vecs_are_equal(patterns[[index]][row,], patterns[[index]][row+1, ])) {
        # Confirm mirrors by comparing outward from adjacent columns
        if (row_is_mirror(patterns[[index]], row)) {
          rows_above_mirror[[index]] <- row
        }

        if (row_is_smudged_mirror(patterns[[index]], row, 0)) {
          rows_above_smudged_mirror[[index]] <- row
        }
      }

      if (vecs_have_one_difference(patterns[[index]][row,], patterns[[index]][row+1, ])) {
        if (row_is_smudged_mirror(patterns[[index]], row, 1)) {
          rows_above_smudged_mirror[[index]] <- row
        }
      }
    }
  }

  p1_result <- sum(cols_left_of_mirror,
                   (rows_above_mirror * 100))

  p2_result <- sum(cols_left_of_smudged_mirror,
                   (rows_above_smudged_mirror * 100))

  print(paste("Part 1:", p1_result))
  print(paste("Part 2:", p2_result))
}

# Parse input
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

# Part one only -----------------------------------------------------------

col_is_mirror <- function(pattern, index) {
  # Assuming immediately adjacent cols have already been checked
  if (index == 1) return(TRUE)

  sep = 3
  for (col1 in (index - 1):1) {
    col2 <- col1 + sep
    sep = sep + 2

    # If all are equal up to the right edge, there is a mirror
    if (col2 > ncol(pattern)) return(TRUE)

    # If any non-matching pairs are found, there is no mirror
    if (!vecs_are_equal(pattern[, col1], pattern[, col2])) {
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

    # If all are equal up to the bottom edge, there is a mirror
    if (row2 > nrow(pattern)) return(TRUE)

    # If any non-matching pairs are found, there is no mirror
    if (!vecs_are_equal(pattern[row1, ], pattern[row2, ])) {
      return(FALSE)
    }

    # If all are equal up to the top edge, there is a mirror
    if (row1 == 1) return(TRUE)
  }
}


# Part two only -----------------------------------------------------------

vecs_have_one_difference <- function(vec1, vec2) {
  return(sum(vec1 == vec2) == length(vec1) - 1)
}

col_is_smudged_mirror <- function(pattern, index, smudge_count) {
  # Assuming immediately adjacent cols have already been checked
  if (index == 1 & smudge_count == 1) return(TRUE)

  sep = 3
  for (col1 in (index - 1):1) {
    col2 <- col1 + sep
    sep <-  sep + 2
    smudge <- FALSE

    # If all are equal up to the right edge, there is a mirror
    if (col2 > ncol(pattern)) {
      return(smudge_count == 1)
    }

    vec1 <- pattern[, col1]
    vec2 <- pattern[, col2]

    # Check for smudges
    if (vecs_have_one_difference(vec1, vec2)) {
      smudge_count = smudge_count + 1
      smudge <-  TRUE
    }

    # If more than one smudge, there is no mirror
    if (smudge_count > 1) return(FALSE)

    # If a non-matching pairs are found without a smudge, there is no mirror
    if (!smudge & !vecs_are_equal(vec1, vec2)) {
      return(FALSE)
    }

    # If all are equal up to the left edge, there is a mirror
    if (col1 == 1) {
      return(smudge_count == 1)
    }
  }
}

row_is_smudged_mirror <- function(pattern, index, smudge_count) {
  # Assuming immediately adjacent cols have already been checked
  if (index == 1 & smudge_count == 1) return(TRUE)

  sep = 3
  for (row1 in (index - 1):1) {
    row2 <- row1 + sep
    sep <-  sep + 2
    smudge <- FALSE

    # If all are equal up to the right edge, there is a mirror
    if (row2 > nrow(pattern)) {
      return(smudge_count == 1)
    }

    vec1 <- pattern[row1, ]
    vec2 <- pattern[row2,]

    # Check for smudges
    if (vecs_have_one_difference(vec1, vec2)) {
      smudge_count = smudge_count + 1
      smudge <-  TRUE
    }

    # If more than one smudge, there is no mirror
    if (smudge_count > 1) return(FALSE)

    # If a non-matching pairs are found without a smudge, there is no mirror
    if (!smudge & !vecs_are_equal(vec1, vec2)) {
      return(FALSE)
    }

    # If all are equal up to the left edge, there is a mirror
    if (row1 == 1) {
      return(smudge_count == 1)
    }
  }
}
