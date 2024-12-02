input <- readLines(here::here("2023", "data", "day6.txt"))

races_raw <- tibble::tibble(
  metric = stringr::str_squish(input)
) |>
  tidyr::separate_wider_delim(
    cols = metric,
    delim = ": ",
    names = c("metric", "value")
  ) |>
  tidyr::separate_longer_delim(
    cols = value,
    delim = " "
  ) |>
  dplyr::mutate(
    race = dplyr::row_number(),
    .by = metric) |>
  dplyr::mutate(
    value = as.integer(value)
  ) |>
  tidyr::pivot_wider(
    names_from = metric,
    values_from = value
  )

ways_to_win <- function(race_duration, length_race) {
  # Quadratic Equation
  x1 <- (race_duration / 2) + sqrt((race_duration / 2)^2 - length_race)
  x2 <- (race_duration / 2) - sqrt((race_duration / 2)^2 - length_race)

  return(abs(diff(c(floor(x1), ceiling(x2)))) + 1)
}

tictoc::tic()
part_1 <- races_raw |>
  dplyr::rowwise() |>
  dplyr::mutate(
    total_ways_to_win = ways_to_win(Time, Distance)
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    total = prod(total_ways_to_win)
  )

part_2 <- races_raw |>
  dplyr::summarise(
    dplyr::across(Time:Distance, \(x) as.numeric(paste0(x, collapse = "")))
  ) |>
  dplyr::mutate(
    total_ways_to_win = ways_to_win(Time, Distance)
  ) 
tictoc::toc()