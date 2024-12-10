input <- as.integer(
  unlist(strsplit(
    readLines(here::here("2024", "d9", "input")),
    ""
  ))
)

# Part 1 ---------
filesystem <- tibble::tibble(
  input = input
) |>
  tibble::rowid_to_column("rowid") |>
  dplyr::mutate(
    file = dplyr::case_when(
      rowid %% 2 == 1 ~ TRUE,
      rowid %% 2 == 0 ~ FALSE
    ),
    id = cumsum(file) - 1,
    id = ifelse(file == FALSE, NA, id)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    col = list(dplyr::case_when(
      rowid %% 2 == 1 ~ rep(id, input),
      rowid %% 2 == 0 ~ rep(NA, input),
    ))
  ) |>
  dplyr::pull(col) |>
  do.call(c, args = _)

free <- which(is.na(filesystem))
diffs <- free - dplyr::lead(free)
while (!all(diffs[!is.na(diffs)] == -1)) {
  move_from <- max(which(!is.na(filesystem)))
  move_to <- min(which(is.na(filesystem)))

  move_what <- filesystem[move_from]

  filesystem[move_to] <- move_what
  filesystem[move_from] <- NA
  free <- which(is.na(filesystem))
  diffs <- free - dplyr::lead(free)
}

checksum <- c()
for (file in seq_along(filesystem)) {
  res_checksum <- as.integer(filesystem[file]) * (file - 1)
  checksum <- c(checksum, res_checksum)
}
part1 <- sum(checksum, na.rm = TRUE)

# Part 2 -------
list_try <- tibble::tibble(
  input = input
) |>
  tibble::rowid_to_column("rowid") |>
  dplyr::mutate(
    file = dplyr::case_when(
      rowid %% 2 == 1 ~ TRUE,
      rowid %% 2 == 0 ~ FALSE
    ),
    id = cumsum(file) - 1,
    id = ifelse(file == FALSE, NA, id)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    col = list(dplyr::case_when(
      rowid %% 2 == 1 ~ rep(id, input),
      rowid %% 2 == 0 ~ rep(NA, input),
    ))
  ) |>
  dplyr::group_by(file) |>
  dplyr::group_split() |>
  purrr::map(\(x) dplyr::pull(x, col))

free_space <- list_try[[1]]
length_space <- lengths(free_space)

files <- list_try[[2]]
length_files <- lengths(files)

for (i in seq(length(files), 1, by = -1)) {
  current_file <- files[[i]]
  length_current_file <- length(current_file)
  fits <- which(length_space >= length_current_file)

  if (min(fits) > i - 1) {
    next
  }

  if (length(fits) == 0) {
    next
  }
  free_space[[min(fits)]][which(is.na(free_space[[min(fits)]]))][seq_len(length_current_file)] <- current_file
  files[i] <- list(rep(NA, length_current_file))
  length_space[min(fits)] <- length_space[min(fits)] - length_current_file
}

new_sort <- c()
for (f in seq_len(10000)) {
  if (all(f > length(files) & f > length(free_space))) {
    next
  } else if (f > length(files)) {
    new_sort <- c(new_sort, free_space[[f]])
  } else if (f > length(free_space)) {
    new_sort <- c(new_sort, files[[f]])
  } else {
    output <- c(files[[f]], free_space[[f]])
    new_sort <- c(new_sort, output)
  }
}
checksum_2 <- c()
for (file in seq_along(new_sort)) {
  res_checksum_2 <- as.integer(new_sort[file]) * (file - 1)
  checksum_2 <- c(checksum_2, res_checksum_2)
}

part2 <- sum(checksum_2, na.rm = TRUE)