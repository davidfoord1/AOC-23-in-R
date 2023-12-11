# Part One ------------------------------------------------------
# shortest_pair_path_total(input)

expand_universe <- function(input) {
  # Regex search for empty columns
  # Contains . from start (^) to end ($)
  empty_columns <- grep("^\\.+$", input)

  # Store the universe to a dataframe
  universe <- strsplit(input, "") |>  as.data.frame()
  names(universe) <- (1:length(universe))

  empty_rows <- numeric()

  row_no <- length(universe[[1]])

  # Regex search for empty rows
  for (row in 1:row_no) {
    if(all(grepl("^\\.+$", universe[row, ]))) {
      empty_rows <- c(empty_rows, row)
    }
  }

  empty_column <- lapply(1:row_no, \(x) ".") |> unlist()

  for (index in seq_along(empty_columns)) {
    col <- empty_columns[[index]] + index - 1
    universe <- cbind(universe[, 1:(col)],
                      empty_column,
                      universe[, (col + 1):length(universe)]
                      )
  }

  empty_row <- lapply(1:length(universe), \(x) ".") |> unlist()

  for (index in seq_along(empty_rows)) {
    row <- empty_rows[[index]] + index - 1
    universe <- rbind(universe[1:(row), ],
                      empty_row,
                      universe[(row + 1):length(universe[[1]]), ]
    )
  }

  universe
}

galaxy_coordinates <- function(universe) {
  coords <- list()

  for (row in 1:nrow(universe)) {
    for (col in 1:ncol(universe)) {
      if (universe[row, col] == "#") {
        coords <- c(coords, list(c(row, col)))
      }
    }
  }

  coords
}

coord_distances <- function(coords) {
  distances <- numeric()

  for (from in 1:(length(coords) - 1)) {
    for (to in (from + 1):length(coords)) {
      distance <- sum(abs(coords[[to]] - coords[[from]]))
      distances <- c(distances, distance)
    }
  }

  sum(distances)
}

shortest_pair_path_total <- function(input) {
  universe <- expand_universe(input)

  coords <- galaxy_coordinates(universe)

  print(coord_distances(coords))
}

# Part Two ------------------------------------------------------
# shortest_pair_path_total2(input)

shortest_pair_path_total2 <- function(input) {
  empty_cols <- empty_columns(input)

  # Store the universe to a dataframe
  universe <- strsplit(input, "") |>  as.data.frame()
  names(universe) <- (1:length(universe))

  empty_rows <- empty_rows(universe)

  coords <- galaxy_coordinates(universe)

  # Replace by n = expand by n - 1
  expand_by <- 1000000 - 1

  coords2 <- lapply(coords,
         function(x) {
           row_counter <- 0
           for (row in empty_rows) {
             if (x[[1]] > row) {
               row_counter <- row_counter + 1
             }
           }

           col_counter <- 0
           for (col in empty_cols) {
             if (x[[2]] > col) {
               col_counter <- col_counter + 1
             }
           }

           row <- x[[1]] + row_counter * expand_by
           col <- x[[2]] + col_counter * expand_by

           return(c(row, col))
         })

  print(coord_distances(coords2))
}

empty_columns <- function(input) {
  # Regex search for empty columns
  # Contains . from start (^) to end ($)
  empty_columns <- grep("^\\.+$", input)
}

empty_rows <- function(universe) {
  empty_rows <- numeric()

  row_no <- length(universe[[1]])



  # Regex search for empty rows
  for (row in 1:row_no) {
    if(all(grepl("^\\.+$", universe[row, ]))) {
      empty_rows <- c(empty_rows, row)
    }
  }

  empty_rows
}
