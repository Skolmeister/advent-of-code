testinput <- c(
"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
)

input <- readLines(here::here("2023", "data", "day2.txt"))

parsed_input <- tibble::tibble(
  string = input
  ) |>
  tidyr::separate_wider_regex(
    cols = string,
    patterns = c(id = ".*", ":", draws = ".*")
  ) |>
  tidyr::separate_longer_delim(
    cols = draws,
    delim = ";"
  ) |>
  dplyr::mutate(
    draws = stringr::str_squish(draws),
    id = as.integer(stringr::str_extract(id, "\\d+")),
    blue = as.integer(stringr::str_extract(draws, "\\d+(?=\\sblue)")),
    red = as.integer(stringr::str_extract(draws, "\\d+(?=\\sred)")),
    green = as.integer(stringr::str_extract(draws, "\\d+(?=\\sgreen)"))
  ) |>
  tidyr::replace_na(
    list(
      blue = 0,
      red = 0,
      green = 0
    )
  ) |>
  dplyr::group_by(id) |> 
  dplyr::summarise(
    dplyr::across(
      blue:green, max
    )
  )

check_tibble <- tibble::tibble(
  blue = 14,
  red = 12,
  green = 13 
)

part_1_result <- parsed_input |>
  dplyr::filter(
    blue <= check_tibble$blue,
    red <= check_tibble$red,
    green <= check_tibble$green,
  ) |>
  dplyr::pull(id) |>
  sum()

part_2 <- parsed_input |>
    dplyr::mutate(
      result = blue * red * green
    ) |>
    dplyr::pull(result) |>
    sum()
