input <- tibble::tibble(input = readLines(here::here("2023", "data", "day7.txt"))) |>
  tidyr::separate_wider_delim(
    input,
    delim = " ",
    names = c("hand", "bid")
  ) |>
  dplyr::mutate(
    bid = as.integer(bid),
    cards = strsplit(hand, "")
  ) |>
  tidyr::unnest_longer(
    col = cards,
    values_to = "single_card",
    indices_to = "card_number"
  ) |>
  dplyr::mutate(
    single_card = as.integer(dplyr::case_when(
      single_card == "A" ~ "14",
      single_card == "K" ~ "13",
      single_card == "Q" ~ "12",
      single_card == "J" ~ "11",
      single_card == "T" ~ "10",
      TRUE ~ single_card
    )
    )
  )

hands_list <- input |>
  dplyr::group_by(hand) |>
  dplyr::group_split()

get_value <- function(hand, part) {
  card_counts <- hand |>
    dplyr::count(single_card) |>
    dplyr::add_count(
      n,
      name = "occurences"
    )
  if (part == "two") {
    if (11 %in% card_counts$single_card) {
      jokers_count <- card_counts[card_counts$single_card == 11, ]$n
      if (jokers_count == 5) {
        return(
          hand |>
          dplyr::mutate(value = 7)
        )
      }
      card_counts_without_joker <- card_counts[card_counts$single_card != 11, ]
      highest_n_value <- max(card_counts_without_joker[card_counts_without_joker$n == max(card_counts_without_joker$n), ]$single_card)
      card_counts <- card_counts_without_joker |>
        dplyr::rowwise() |>
        dplyr::mutate(
          n = dplyr::case_when(
            all(single_card != 11, single_card == highest_n_value) ~ n + jokers_count,
            TRUE ~ n
          )
        )
    }
  }

  hand |>
    dplyr::mutate(
      value = dplyr::case_when(
    5 %in% card_counts$n ~ 7,
    4 %in% card_counts$n ~ 6,
    3 %in% card_counts$n && 2 %in% card_counts$n ~ 5,
    3 %in% card_counts$n ~ 4,
    2 %in% card_counts$n && unique(card_counts[card_counts$n == 2,]$occurences) == 2 ~ 3,
    2 %in% card_counts$n ~ 2,
    TRUE ~ 1
    )
  )

}

part_1 <- purrr::map_df(hands_list, get_value, part = "one") |>
  tidyr::pivot_wider(
    names_from = card_number,
    values_from = single_card,
    names_prefix = "card_"
  ) |>
  dplyr::arrange(
    dplyr::across(c(value, tidyselect::contains("card_")))
  ) |>
  dplyr::mutate(
    rank = dplyr::row_number(),
    winning = bid * rank
  ) |>
  dplyr::pull(winning) |>
  sum()

part_2 <- purrr::map_df(hands_list, get_value, part = "two") |>
  dplyr::mutate(
    single_card = dplyr::case_when(
      single_card == 11 ~ 1,
      TRUE ~ single_card
    )
  ) |>
  tidyr::pivot_wider(
    names_from = card_number,
    values_from = single_card,
    names_prefix = "card_"
  ) |>
  dplyr::arrange(
    dplyr::across(c(value, tidyselect::contains("card_")))
  ) |>
  dplyr::mutate(
    rank = dplyr::row_number(),
    winning = bid * rank
  ) |> 
  dplyr::pull(winning) |>
  sum()

