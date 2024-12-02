input <- readLines(here::here("2023", "data", "day4.txt"))

parsed_input <- strsplit(trimws(gsub("Card\\s+\\d+:", "", input)), "\\s\\|\\s")

extract_numbers <- function(number_string) {
  as.integer(unlist(strsplit(number_string, "\\s+")))
}

calculate_points <- function(wins) {
  if (wins > 0) {
    points <- 2^(wins - 1)
  } else {
    points <- 0
  }
  return(points)
}

extract_matches <- function(card) {
  winning_numbers <- extract_numbers(card[1])
  drawn_numbers <- extract_numbers(card[2])

  wins <- sum(drawn_numbers %in% winning_numbers)
  return(wins)
}

part_1 <- sum(
  purrr::map_dbl(
    purrr::map_dbl(
      parsed_input,
      extract_matches
    ),
    calculate_points
  )
)

all_matches <- tibble::tibble(
  matches = purrr::map_dbl(parsed_input, extract_matches),
  count = 1
)

tictoc::tic()

for (i in seq_len(nrow(all_matches))) {
  if (all_matches$matches[i] > 0) {
    all_matches$count[get_new_cards(i)] <- (all_matches$count[get_new_cards(i)] + all_matches$count[i])
  }
}

part_2 <- sum(all_matches$count)