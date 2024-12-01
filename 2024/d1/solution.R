tictoc::tic()
input <- tibble::tibble(
  input = readLines(here::here("2024", "d1", "input"))
) |>
  tidyr::separate_wider_regex(
    input, 
    patterns = c(list1 = "\\d+", "\\s+", list2 = "\\d+")
  )

part1 <- input |>
  dplyr::mutate(
    list1 = as.integer(sort(list1)),
    list2 = as.integer(sort(list2)),
    diff = abs(list1 - list2)
  ) |>
  dplyr::pull(diff) |>
  sum()


part2 <- input |>
  dplyr::filter(
    list2 %in% list1
  ) |>
  dplyr::count(list2) |>
  dplyr::mutate(
    similarity_score = as.integer(list2) * n
  ) |>
  dplyr::pull(similarity_score) |>
  sum()
tictoc::toc()
