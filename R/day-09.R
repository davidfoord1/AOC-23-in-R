# Parts One and Two ---------------------------------------------

# For Part One:
# extrapolated_sum(input)
# For Part Two
# extrapolated_sum(input, reverse = TRUE)

next_number <- function(sequence) {
  # Initialise for iteration
  sequence_steps <- list(sequence)
  iteration_sum <- -1
  iteration_count <- 1

  # Loop until all values are 0
  while(iteration_sum != 0) {
    # Get the starting sequence or previous sequence of differences
    iteration_steps <- sequence_steps[[iteration_count]]

    # Initialise for iteration
    iteration_differences <- numeric()

    # From the second number to the last number
    for (index in 2:length(iteration_steps)) {
      # Get the difference from the previous number
      difference <- iteration_steps[[index]] - iteration_steps[[index - 1]]
      # Store the differences in a vector
      iteration_differences <- c(iteration_differences, difference)
    }

    # Store the vector in the list
    sequence_steps <- c(sequence_steps, list(iteration_differences))

    # Sum for the check if all values are 0
    iteration_sum <- sum(iteration_differences)

    # Increase the iteration count
    # So that the next set of differences will be based in this one
    iteration_count = iteration_count + 1
  }

  # Finally, add the last value per row to the list
  final_values <- lapply(sequence_steps, \(x) x[[length(x)]])
  next_number <- Reduce(`+`, final_values)
}

extrapolated_sum <- function(input, reverse = FALSE) {
  # Create a list of vectors from each line of input
  sequences <- input |>
    lapply(function(line) {
      line <- str_extract_numbers(line, "-*\\d+")
    })

  # Reverse the vectors for Part Two
  if (reverse) {
    sequences <- lapply(sequences, rev)
  }

  # Get the next number of each vector and sum
  numbers <- map_num(sequences, next_number)
  print(sum(numbers))
}

