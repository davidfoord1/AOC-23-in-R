# Part One ------------------------------------------------------

almanac_seeds <- function(almanac) {
  strings <- almanac[[1]] |> strsplit(split = " ") |> unlist()

  strings <- strings[2:length(strings)]

  seeds <- as.numeric %.% strings
}

get_section_mapping <- function(almanac, start_line, end_line) {
  if (end_line > 0) {
    almanac_section <- almanac[start_line:end_line]
  } else {
    # For the last section where end_line == -1
    almanac_section <- almanac[start_line:length(almanac)]
  }

  sub_maps <- lapply(almanac_section,
                     function(line) {
                       values = line |> strsplit(split = " ") |> unlist()

                       sub_map <- data.frame(
                         dest_start = as.numeric %.% values[[1]],
                         source_start = as.numeric %.% values[[2]],
                         range_length = as.numeric %.% values[[3]]
                       )

                       return(sub_map)
                     })

  section_map <- Reduce(rbind, sub_maps)
}

almanac_section_locations <- function(almanac) {
  # All section starts
  section_starts <- grep("-", almanac) + 1

  # Each section ends three lines before the start of the next one
  section_ends <- section_starts[2:length(section_starts)] - 3

  # Special case for the last section
  section_ends[[length(section_starts)]] <- -1

  sections_df <- data.frame(start_line = section_starts,
                            end_line = section_ends)
}

almanac_mappings <- function(almanac) {
  sections_df <- almanac_section_locations(almanac)

  mappings <- list()

  for (row in 1:nrow(sections_df)) {
    mappings[[row]] <- get_section_mapping(almanac,
                                    sections_df[row, 1],
                                    sections_df[row, 2])
  }

  return(mappings)
}

seed_to_location <- function(item_number, mappings, map_no = 1) {
  # Return if no more mappings
  if (map_no > length(mappings)) {
    return(item_number)
  }

  map_df <- mappings[[map_no]]

  for (row in 1:nrow(map_df)) {
    start <- map_df[row, ]$source_start
    end <- start + map_df[row, ]$range_length - 1

    distance <- item_number - start

    # If the source number in the range of the mapped numbers
    # Proceed with the corresponding destination number
    # Otherwise keep the source number as the destination number
    if (item_number >= start & item_number <= end) {
      item_number <- map_df[row, ]$dest_start + distance
      break
    }
  }

  # Iterate mapping for next recursive call
  map_no <- map_no + 1

  return(seed_to_location(item_number, mappings, map_no))
}

smallest_seed_location <- function(almanac) {
  mappings <- almanac_mappings(almanac)

  seeds <- almanac_seeds(almanac)

  locations <- min %.% map_num(seeds,
                       function(seed) {
                         seed_to_location(seed, mappings)
                       })
}

solve_day_05_p1 <- function() {
  input <- load_real_data("05")

  print(smallest_seed_location(input))
}

