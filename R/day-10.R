# Parts One and Two ------------------------------------------------------
# solve_day_10(input)


parse_pipes <- function(input, pipe_ref) {
  connections <- strsplit(input, "")

  for (line in seq_along(connections)) {
    connections[[line]] <- pipe_ref[connections[[line]]]
  }

  connections <- connections |> lapply(as.character)

  return(connections)
}

find_position <- function(parsed_input, search = "NE \\*") {
  col <- grep(search, parsed_input)
  row <- grep(search, parsed_input[[col]])

  pos <- c(row, col)
}

solve_day_10 <- function(input) {
  pipe_ref <- list(
    "S" = "NE *",
    "|" = "NS",
    "-" = "EW",
    "L" = "NE",
    "J" = "NW",
    "7" = "SW",
    "F" = "SE",
    "." = NA
  )

  dir_ref <- list(
    "N" = c(0, -1),
    "S" = c(0, 1),
    "E" = c(1, 0),
    "W" = c(-1, 0)
  )

  connections <- parse_pipes(input, pipe_ref)

  connections <- connections |> as.data.frame()

  start_pos <- find_position(connections)

  pos <- find_connection_to_start(connections, start_pos, dir_ref)

  prev_pos <- start_pos
  step_counter <- 1

  while(!pos_equals(pos, start_pos)) {
    connections[pos[[1]], pos[[2]]] <- paste(connections[pos[[1]], pos[[2]]], "*")
    print(paste("step:", step_counter, "-", connections[[pos[[1]], pos[[2]]]]))
    step_counter <- step_counter + 1
    temp <- pos
    pos <- find_next_pos(connections, pos, prev_pos, dir_ref)
    prev_pos <- temp
  }

  names(connections) <- (1:length(connections))

  enclosed_count <- count_enclosed_tiles(connections, pipe_ref)

  print(paste("P1 Furthest steps:", ceiling(step_counter/2)))

  print(paste("P2 Enclosed tile counts:", enclosed_count))
}

find_connection_to_start <- function(connections, start_pos, dir_ref) {
  next_pos <- c(0, 0)

  for (row in -1:1) {
    if (start_pos[[1]] + row < 1) next

    for (col in -1:1) {
       # Skip if back at the start
      if (row == 0 & col == 0) next

      if (start_pos[[2]] + col < 1) next

      pos <- start_pos + c(row, col)
      val <- connections[pos[[1]], pos[[2]]]

      print(paste(pos))

      if (val != "NA") {
        dir1 <- substr(val, 1, 1)
        dir2 <- substr(val, 2, 2)

        dir1 <- dir_ref[[dir1]]
        dir2 <- dir_ref[[dir2]]

        if (pos_equals(pos + dir1, start_pos) | pos_equals(pos + dir2, start_pos)) {
          print(paste(pos, "connects to start"))
          next_pos <- pos
          break
        }
      }
    }
  }

  if(pos_equals(next_pos, c(0, 0))) stop("Invalid next pos")

  next_pos
}

find_next_pos <- function(connections, pos, prev_pos, dir_ref) {
  val <- connections[pos[[1]], pos[[2]]]

  dir1 <- substr(val, 1, 1)
  dir2 <- substr(val, 2, 2)

  dir1 <- dir_ref[[dir1]]
  dir2 <- dir_ref[[dir2]]

  if (pos_equals(pos + dir1, prev_pos)) {
    pos <- pos + dir2
  } else {
    pos <- pos + dir1
  }

  print(paste(pos))
  pos
}


pos_equals <- function(one, two) {
  if(one[[1]] == two[[1]] & one[[2]] == two[[2]]) {
    return(TRUE)
  }
  return(FALSE)
}

count_enclosed_tiles <- function(connections, pipe_ref) {
  # Starting at the outside, mark tiles until you meet the loop
  # Every time you cross the loop's border, alternate between
  # Marking as inside or outside
  # Count the number of tiles marked as inside


  # To count crossings correctly
  # Only count those completely perpendicular to your direction of travel
  # And those pointing in one just one of the perpendicular directions
  # So I just checked for `|` (NS), `L` (NE), or `J` (NW)
  # Ignoring `-`, `F`, `7`
  # This is to ensure correct handling of travelling along the border:

  # O O O L - - - J O O O O O ~ Flip twice (even) remain outside

  # O O O F - - - 7 O O O O O ~ Flip no times (even) remain outside

  # O O O L - - - 7 I I I I I ~ Flip once (odd) move inside

  # O O O F - - - J I I I I I ~ Flip once (odd) move inside

  counter <- 0
  for (col in 1:ncol(connections)) {
    mark = "O"

    for (row in 1:nrow(connections)) {

      val <- connections[row, col]

      if (grepl("(NS|NE|NW) \\*$", val)) {
        mark <- switch (mark,
                        "O" = "I",
                        "I" = "O"
      )}

      if (!grepl("\\*$", val)) {
        connections[row, col] <- mark
        if (mark == "I") counter <- counter + 1
      }
    }
  }

  return(counter)
}
