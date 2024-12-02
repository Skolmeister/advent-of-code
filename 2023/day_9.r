
input_raw <- lapply(
  stringr::str_extract_all(
    readLines(here::here("2023", "data", "day9.txt")),
    "[\\-]?\\d+"
  ),
  FUN = as.integer
)

parse_sequences <- function(sequence) {
  seq_list <- list(sequence)

  while (!all(sequence == 0)) {
    sequence <- diff(sequence)
    seq_list[[length(seq_list) + 1]] <- sequence
  }
  return(seq_list)
}

parsed_seq <- purrr::map(input_raw, parse_sequences)

extrapolate_seq <- function(sequence) {
  for (seq in rev(seq_len(length(sequence) - 1))) {
    l_seq <- length(sequence[[seq]])
    new_value <- sequence[[seq]][l_seq] + tail(sequence[[seq + 1]], 1)

    sequence[[seq]] <- append(sequence[[seq]], new_value)

    if (seq == 1) {
      return(new_value)
    }
  }
}

part_1 <- sum(unlist(purrr::map(parsed_seq, extrapolate_seq)))

extrapolate_backwards <- function(sequence) {
  for (seq in rev(seq_len(length(sequence) - 1))) {
    l_seq <- length(sequence[[seq]])
    new_value <- sequence[[seq]][1] - sequence[[seq + 1]][1]
    sequence[[seq]] <- append(new_value, sequence[[seq]])

    if (seq == 1) {
      return(new_value)
    }
  }
}

part_2 <- sum(unlist(purrr::map(parsed_seq, extrapolate_backwards)))
