# Part One:
# steps_in_input(input)
# Part Two
# minimum_cycle_steps(input)

steps_in_input <- function(input) {
  instructions <- strsplit(input[[1]], "") |> unlist()

  node_input <- input[3:length(input)]

  nodes <- list()

  for(line_no in seq_along(node_input)) {
      line <- gsub("[[:punct:]]", "", node_input[[line_no]]) |>
        strsplit(" +") |>
        unlist()

      nodes[[line[[1]]]] <- c(line[[2]], line[[3]])
  }


  current_node <- "AAA"

  print(steps_from_node(current_node, nodes, instructions))
}

minimum_cycle_steps <- function(input) {
  instructions <- strsplit(input[[1]], "") |> unlist()

  node_input <- input[3:length(input)]

  nodes <- list()

  for(line_no in seq_along(node_input)) {
    line <- gsub("[[:punct:]]", "", node_input[[line_no]]) |>
      strsplit(" +") |>
      unlist()

    nodes[[line[[1]]]] <- c(line[[2]], line[[3]])
  }

  current_nodes <- node_input |>
    lapply(
      function(line) {
        node <- substr(line, 1, 3)
      })

  current_nodes <- grep("..A", current_nodes, value = TRUE)

  steps_per_node <- current_nodes |>
    lapply(function(node) steps_from_node(node, nodes, instructions)) |>
    unlist()

  # Find the lowest common multiple


  lcm <- Reduce(function(x1, x2) lowest_common_multiple(x1, x2),
                 steps_per_node)

  # Quick hack for non-scientifc notation
  write.csv(lcm)
}

steps_from_node <- function(start_node, nodes, instructions) {
  step_counter <-  0
  current_node <- start_node
  queue <- instructions

  while(!grepl("..Z", current_node)) {
    if (is.na(queue[[1]])) {
      queue <- instructions
    }

    if (queue[[1]] == "L") {
      current_node = nodes[[current_node]][[1]]
    } else {
      current_node = nodes[[current_node]][[2]]
    }

    step_counter <- step_counter + 1

    queue <- queue[2:length(queue)]
  }

  return(step_counter)
}

lowest_common_multiple <- function(n1, n2) {
  r <- abs(n1*n2)/greatest_common_denominator(n1, n2)
}

greatest_common_denominator <- function(n1, n2) {
  # Euclidean algorithm

  # Switch number if second input is bigger
  if (n2 > n1) {
    temp <- n1
    n1 <- n2
    n2 <- temp
  }

  while (n2 > 0){
    # Temporarily store the bigger number for this iteration
    temp <- n1
    # Store the smaller number
    # (switched to be the bigger for the next iteration)
    n1 <- n2
    # Get the remained of the bigger number / the smaller number
    n2 <- temp %% n2

    # Forget the bigger number, the smaller number becomes
    # the next iteration's bigger number
    # Repeat until
  }

  n1
}

