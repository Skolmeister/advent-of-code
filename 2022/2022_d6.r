# Day 6 Advent of Code

input_path <- here::here("2022", "input", "2022_d6_input.txt")
input <- readr::read_lines(input_path)

get_marker <- function(input, marker = "start-of-packet") {
    split_string <- unlist(strsplit(input, ""))

    for (i in seq_along(split_string)) {
        if (marker == "start-of-packet") { # Length of unique chars
            length_unique <- 4
        } else if (marker == "start-of-message") {
            length_unique <- 14
        }

        unique_string <- unique(split_string[i:(i + length_unique - 1)])

        if (length(unique_string) == length_unique) { # Break the loop when all are unique
            cat(paste0("Marker ", marker, " at ", i + length_unique - 1, "!\n"))
            break
        }
    }
}

get_marker(input, marker = "start-of-packet")
get_marker(input, marker = "start-of-message")
