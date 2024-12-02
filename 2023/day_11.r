input_raw <- strsplit(readLines(here::here("2023", "data", "day11.txt")), "")

input_mat <- matrix(unlist(input_raw), ncol = length(input_raw)[[1]], byrow = TRUE) |>
  dplyr::as_tibble(.name_repair = "unique")

cols_to_expand <- input_mat |>
  dplyr::summarise(
    dplyr::across(tidyselect::everything(), \(x) sum(x == "."))
  )

cols <- which(cols_to_expand == max(cols_to_expand[1,])) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    col_count = dplyr::row_number()
  ) |>
  dplyr::rename(
    "col_adj" = value
  )

rows_to_expand <- input_mat |>
  dplyr::mutate(
    row = dplyr::row_number()
  ) |>
  dplyr::filter_at(
    dplyr::vars(!tidyselect::starts_with("row")),
    dplyr::all_vars(. == ".")) |>
  dplyr::mutate(
    count = dplyr::row_number()
  ) |>
  dplyr::select("row_adj" = row, "row_count" = count)

all_galaxies_before <- which(input_mat == "#", arr.ind = TRUE) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    id = dplyr::row_number()
  ) |>
  dplyr::left_join(
    cols, by = dplyr::join_by(
       closest(col >= col_adj)
    )
  ) |>
  dplyr::left_join(
    rows_to_expand, by = dplyr::join_by(
       closest(row >= row_adj)
    )
  ) |>
  tidyr::replace_na(
    list(
      row_count = 0,
      col_count = 0
    )
  )


part_1_galaxies <- all_galaxies_before |>
  dplyr::mutate(
    row_new = row + row_count,
    col_new = col + col_count
  ) |>
  dplyr::select(
    row_new, col_new, id
  )

part_1 <- sum(dist(part_1_galaxies[, c(1, 2)], method = "manhattan"))

expand_by <- 1000000
part_2_galaxies <- all_galaxies_before |>
  dplyr::mutate(
    row_new = row + (row_count * (expand_by - 1)),
    col_new = col + (col_count * (expand_by - 1))
  ) |>
  dplyr::select(
    row_new, col_new, id
  )

part_2 <- sum(dist(part_2_galaxies[, c(1,2)], method = "manhattan"))
