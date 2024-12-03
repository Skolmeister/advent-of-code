leaderboard <- "https://adventocode.com/2024/leaderboard/private/view/1032765.json"
session <- dotenv::load_dot_env()
jsonlite::fromJSON(leaderboard)
stars <- httr::GET(
  "https://adventofcode.com/2023/leaderboard/private/view/1032765.json",
  httr::accept_json(),
  httr::user_agent("Alex Nickel https://github.com/Skolmeister")
)

req <- httr2::request(leaderboard) |>
  httr2::req_headers(
    cookie = glue::glue("session={Sys.getenv('AOC_SESSION2024')}"),
    `user-agent` = "Alex Nickel https://github.com/Skolmeister",
    accept = "application/json"
  )
resp <- req |>
  httr2::req_perform()

json_output <- resp |>
  httr2::resp_body_json()

members <- json_output[[1]]

extract_leaderboard <- function(entry) {
  if (!"name" %in% names(unlist(entry))) {
    return()
  }

  tibble::tibble(
    id = entry$id,
    name = entry$name,
    stars = entry$stars,
    local_score = entry$local_score,
    completion_day_level = list(entry$completion_day_level)
  )
}

extracted <- purrr::map_df(members, extract_leaderboard) |>
  tidyr::unnest_longer(
    completion_day_level,
    values_to = "completion_day_level",
    indices_to = "day"
  ) |>
  tidyr::unnest_wider(completion_day_level) |>
  tidyr::unnest_wider(c(`1`, `2`), names_sep = "_") |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::contains("ts"),
      \(x) lubridate::as_datetime(x, tz = "Europe/Berlin")
    )
  )

single_day <- extracted |>
  dplyr::select(
    day,
    name,
    id,
    stars,
    local_score,
    "time_to_first_star" = `1_get_star_ts`,
    "time_to_second_star" = `2_get_star_ts`
  ) |>
  dplyr::mutate(
    time_between_stars = time_to_second_star - time_to_first_star
  )

statistic <- single_day |>
  dplyr::group_by(
    id, name
  ) |>
  dplyr::summarize(
    stars = min(stars),
    local_score = min(local_score),
    mean_time = mean(time_between_stars, na.rm = TRUE),
    median_time = median(time_between_stars, na.rm = TRUE)
  ) |>
  dplyr::arrange(desc(local_score))

user_statistic <- list(
  daily = single_day |>
    dplyr::filter(name == "Alex N"),
  total = statistic |>
    dplyr::filter(
      name == "Alex N"
    )
)

# Scoring based on completion time of part 2

single_day |>
  dplyr::select(name, day, time_between_stars) |>
  dplyr::filter(!is.na(time_between_stars)) |>
  dplyr::group_by(day) |>
  dplyr::mutate(
    rank = dplyr::min_rank(time_between_stars),
    points = 101 - rank
  ) |> 
  dplyr::ungroup() |>
  dplyr::summarise(
    points = sum(points),
    .by = "name"
  ) |>
  dplyr::arrange(desc(points))

