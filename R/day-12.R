parse_spings <- function(input) {
  arrangements <- input |> strsplit(" ")

  # Split input into lists of strings/springs and broken spring group numbers
  strings <- arrangements |>
    lapply(\(x) x[[1]])

  groups_list <- arrangements |>
    lapply(\(x) strsplit(x[[2]], ",") |> unlist())

  arrangements_found <- numeric(length(arrangements))

  for (index in 1:length(arrangements)) {
    arrangements_found[[index]] <-  try_arrangement(strings[[index]], groups_list[[index]])
  }

  print(sum(arrangements_found))
}

is_valid_arrangement <- function(string, groups) {
  locations <- gregexpr("#+", string)

  lengths <- locations |>
    lapply(\(x) attr(x, "match.length")) |>
    unlist()

  if(length(lengths) != length(groups)) {
    return(FALSE)
  }

  return(prod(lengths == groups) == 1)
}

try_arrangement <- function(string, groups) {
  print(paste("Trying", string))
  q_marks <- gregexpr("\\?", string) |> unlist()

  if (!q_marks[[1]] == -1) {
    q_marks <- length(q_marks)
  }

  if(q_marks < 1 & is_valid_arrangement(string, groups)) {
    print("1 found")
    return(1)
  }

  if (q_marks < 1) return(0)

  poss_1 <- try_arrangement(sub("\\?", ".", string), groups)
  poss_2 <- try_arrangement(sub("\\?", "#", string), groups)

  return(sum(poss_1, poss_2))
}
