game_parser <- function(string) {
  id_regex <- "(?<=Game )[0-9]+(?=:)"

  # Please just use stringr::str_extract()...
  id <- regmatches(string, m = regexpr(id_regex, string, perl = TRUE))

  colours <- c("red", "green", "blue")

  highest_counts <- lapply(colours,
         function(colour) {
           regex <- paste0("(?<=(:|;|,) )[0-9]+(?= ", colour, ")")

           # Extract all matches
           counts <- regmatches(string, m = gregexpr(regex, string, perl = TRUE)) |>
             unlist() |>
             as.numeric()

           max <- max(counts)
         }) |>
    unlist()

  data <- data.frame(
    id = id,
    red_highest = highest_counts[[1]],
    green_highest = highest_counts[[2]],
    blue_highest = highest_counts[[3]]
  )
}

is_possible_game <- function(game, max_cubes) {
  colours <- c("red", "green", "blue")

  possible <- lapply(colours,
         function(colour) {
           colour_max <- max_cubes[[paste0(eval(colour), "_max")]]
           colour_highest <- game[[paste0(eval(colour), "_highest")]]

           if(colour_highest > colour_max) {
              return(FALSE)
           } else {
             return(TRUE)
           }
         }) |>
    # Unlist to logical vector
    unlist()

  return(sum(possible) == 3)
}

possible_id_sum <- function(input, max_cubes) {
  ids <- input |>
    lapply(
      function(string) {
        game <- game_parser(string)
        if(is_possible_game(game, max_cubes)) {
          return(game$id)
        } else {
          return(0)
        }
      }
    ) |>
    unlist() |>
    as.numeric()

  return(sum(ids))
}

solve_day_02_p1 <- function() {
  input <- read_input_txt("day-02.txt")

  max_cubes <- data.frame(
    red_max = 12,
    green_max = 13,
    blue_max = 14
  )

  return(possible_id_sum(input, max_cubes))
}


# Part Two ------------------------------------------------------

power_of_min_cubes <- function(game) {
  colours <- c("red", "green", "blue")

  # Extract counts from dataframe to numeric vector
  min_counts <- lapply(colours,
      function(colour) {
        return(game[[paste0(eval(colour), "_highest")]])
      }) |>
    unlist() |>
    as.numeric()

  power <- Reduce(`*`, min_counts)
}

power_of_cubes_sum <- function(input) {
 powers  <- input |>
    lapply(function(string) {
      power_of_min_cubes(game_parser(string))
      }) |>
    unlist() |>
    as.numeric()

  return(sum(powers))
}

solve_day_02_p2 <- function() {
  input <- read_input_txt("day-02.txt")

  return(power_of_cubes_sum(input))
}


