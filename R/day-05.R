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


# Part Two ------------------------------------------------------

almanac_seed_ranges <- function(almanac) {
  string <- almanac[[1]]
  pair_regex <- "\\d+ \\d+"

  pairs <- string |>
    regmatches(m = gregexpr(pair_regex,
                            string,
                            perl = TRUE)) |>
    unlist()

  pairs <- strsplit(pairs, split = " ")

  pairs <- lapply(pairs, as.numeric)
}

smallest_seed_location2 <- function(almanac) {
  mappings <- almanac_mappings %.% almanac

  seed_ranges <- almanac_seed_ranges %.% almanac

  range_locations <- lapply(seed_ranges,
         function(range) {
           return %.% min %.% unlist %.% smallest_range_location(range, mappings)
         })

  min_location <- min %.% unlist %.% range_locations
}

smallest_range_location <- function(input_range, mappings, map_no = 1) {
  # Recursive function which exits here -
  # When an input has passed through all mapping groups;
  # The final location range has been found;
  # As we are looking for the smallest location from the input range;
  # We just return the start position:
  if (map_no > length(mappings)) {
    return(input_range[[1]])
  }

  # Store the inputs as a list of ranges
  ranges_to_check <- list(input_range)
  output_ranges <- list()

  # Get the mappings for the current mapping group
  map_df <- mappings[[map_no]]

  # Each row in map_df is one mapped range to check against
  for (row in 1:nrow(map_df)) {
    remaining_ranges <- list()

    for (range_to_check in ranges_to_check) {
      # Get the bounds of the input range to check
      pending_start <- range_to_check[[1]]
      pending_length <- range_to_check[[2]]
      pending_end <- pending_start + pending_length - 1

      # Get the bounds of the mapped range to check against
      source_start <- map_df[row, ]$source_start
      range_length <- map_df[row, ]$range_length
      source_end <- source_start + range_length - 1
      dest_start <- map_df[row, ]$dest_start

      # Check for intersection between input and map ranges.
      # Handle ranges Before, In, After or Not intersecting differently.
      if (!(pending_end < source_start | pending_start > source_end)) {
        # Range Before Intersection
        # If any of the input is before the source range
        # Store it as remaining to be checked again on the next iteration

        # Example:
        # input         |-----|
        # map-source       |-----|
        #
        # remaining     |--|
        if (pending_start < source_start) {
          pre_intersect_length <- source_start - pending_start
          pre_intersect_range <- c(pending_start, pre_intersect_length)

          remaining_ranges <- c(remaining_ranges, list(pre_intersect_range))
        }

        # Intersection
        # Where there is an intersection derive the intersection range
        # Then map it to the destination range
        # Append the new range to the output ranges

        # Example:
        # input       |------|
        # map-source      |-----|
        # map-dest                 |-----|
        #
        # intersects      |--|
        # output                   |--|
        intersect_start <- max(pending_start, source_start)
        intersect_end <- min(pending_end, source_end)
        intersect_length <- intersect_end - intersect_start + 1

        mapped_start <- dest_start + (intersect_start - source_start)
        mapped_range <- c(mapped_start, intersect_length)

        output_ranges <- c(output_ranges, list(mapped_range))

        # Range Before Intersection
        # If any of the input is after the source range
        # Store it as remaining to be checked again on the next iteration

        # Example:
        # input               |-------|
        # map-source       |-----|
        #
        # remaining              |----|
        if (pending_end > source_end) {
          post_intersect_start <- source_end + 1
          post_intersect_length <- pending_end - post_intersect_start + 1
          post_intersect_range <- c(post_intersect_start, post_intersect_length)

          remaining_ranges <- c(remaining_ranges, list(post_intersect_range))
        }
      } else {

        # No Intersection
        # Keep the ranges to check as is

        # Example:
        # input                     |-------|
        # map-source       |-----|
        #
        # remaining                 |-------|
        remaining_ranges <- c(remaining_ranges, list(range_to_check))
      }
    }

    # Set the ranges to be checked against the next mapped range
    ranges_to_check <- remaining_ranges
  }

  # Any ranges that have not had an intersection with any of the mapped ranges
  # Gets added to the output ranges as is
  output_ranges <- c(output_ranges, ranges_to_check)

  # Iterate the mapping group for the next recursive call
  map_no <- map_no + 1

  # Pass all out_ranges back into this function as inputs
  # To be checked against the next mapping group
  lapply(output_ranges,
         function(output) {
           smallest_range_location(output, mappings, map_no)
         })
}



solve_day_05_p2 <- function() {
  input <- load_real_data("05")

  print(smallest_seed_location2(input))
}

