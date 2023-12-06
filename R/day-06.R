race_records <- function(input) {
  race_time <- str_extract_numbers(input[[1]])
  race_distance <- str_extract_numbers(input[[2]])

  race_records <- matrix(c(race_time, race_distance),
                         ncol = 2)
}

valid_races <- function(race_time) {
  valid_races <- matrix(nrow = race_time - 1, ncol = 3)

  for (speed in 1:race_time - 1) {
    # Speed
    valid_races[speed, 1] <- speed

    # Duration
    duration <- race_time - speed
    valid_races[speed, 2] <- duration

    # Distance
    valid_races[speed, 3] <- speed * duration
  }

  valid_races
}

count_record_beaters <- function(input) {
  race_records <- race_records(input)

  record_beaters <- numeric()

  for (row in 1:nrow(race_records)) {
    distances <- valid_races(race_records[row, 1])[, 3]
    record <- race_records[row, 2]

    record_beaters <- c(record_beaters, sum(distances > record))
  }

  return(Reduce(`*`, record_beaters))
}

# Part Two ------------------------------------------------------

race_record <- function(input) {
  race_time <- str_extract_numbers(input[[1]]) |>
    paste(collapse = "") |> as.numeric()

  race_record <- str_extract_numbers(input[[2]]) |>
    paste(collapse = "") |> as.numeric()

  # Use quadratic formula to find the two points
  # Considering x as speed, y as distance
  # Find the value of x, where y = race_record
  # From
  # 0 = ax^2 + bx + c
  # b = race_time
  # c = -race_record

  # Plug into quadratic formula
  plus_minus <- sqrt((race_time ^ 2) - (4 * (race_record)))

  max_speed <- floor((race_time + plus_minus)/2)
  min_speed <- ceiling((race_time - plus_minus)/2)

  # The number of points between these values
  # is the number of ways to beat the record
  print(max_speed - min_speed + 1)
}
