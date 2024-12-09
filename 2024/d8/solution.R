input <- readLines(here::here("2024", "d8", "input"))

input_matrix <- input |>
  strsplit("") |>
  unlist() |>
  matrix(
    nrow = length(input),
    ncol = nchar(input[1]),
    byrow = TRUE
  )

input_frequencies <- names(table(input_matrix))
frequencies <- input_frequencies[input_frequencies != "."]

create_antinodes <- function(frequency, mat, part) {
  antennas <- which(mat == frequency, arr.ind = TRUE)

  if (part == 1) {
    antinodes <- purrr::map(
      seq_len(nrow(antennas)),
      \(x) get_antinode_indexes(x, antennas)
    )
  } else if (part == 2) {
    antinodes <- purrr::map(
      seq_len(nrow(antennas)),
      \(x) get_antinode_indexes_2(x, antennas)
    )
  }

  antinodes |>
    do.call(args = _, what = rbind) |>
    dplyr::as_tibble() |>
    dplyr::filter(
      dplyr::between(row, 1, nrow(mat)),
      dplyr::between(col, 1, ncol(mat))
    ) |>
    dplyr::anti_join(
      dplyr::as_tibble(antennas),
      by = c("row", "col")
    )
}

get_antinode_indexes <- function(antenna, all_antennas) {
  move_first <- matrix(all_antennas[antenna, ] - as.vector(t(all_antennas)), ncol = 2, byrow = TRUE)
  move_second <- move_first * 2

  first_node <- matrix(all_antennas[antenna, ] + as.vector(t(move_first)), ncol = 2, byrow = TRUE)
  second_node <- matrix(all_antennas[antenna, ] - as.vector(t(move_second)), ncol = 2, byrow = TRUE)

  antinodes <- rbind(
    first_node,
    second_node
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      "row" = V1,
      "col" = V2
    )

  return(antinodes)
}

part1 <- purrr::map_df(
  frequencies,
  \(x) create_antinodes(x, input_matrix, part = 1)
) |>
  dplyr::distinct(row, col) |>
  nrow()

get_antinode_indexes_2 <- function(antenna, all_antennas) {
  moves <- matrix(all_antennas[antenna, ] - as.vector(t(all_antennas)), ncol = 2, byrow = TRUE)

  start_pos <- all_antennas[antenna, ]
  indices <- tibble::tibble(
    row = start_pos[1],
    col = start_pos[2]
  )

  for (move in seq_len(nrow(moves))) {
    if (all(moves[move, ] == c(0, 0))) {
      next
    }
    rows_down <- start_pos[1] + moves[move, 1]
    rows_up <- abs(nrow(input_matrix) / moves[move, 1])


    if (rows_down > 0) {
      for (r in seq_len(rows_down)) {
        node <- start_pos + (moves[move, ] * r)
        indices <- indices |>
          dplyr::add_row(
            row = node[1],
            col = node[2]
          )
      }
    }

    if (rows_up > 0) {
      for (u in seq_len(rows_up)) {
        node <- start_pos - (moves[move, ] * u)
        indices <- indices |>
          dplyr::add_row(
            row = node[1],
            col = node[2]
          )
      }
    }
  }

  return(indices)
}

part2 <- purrr::map_df(
  frequencies,
  \(x) create_antinodes(x, input_matrix, part = 2)
) |>
  dplyr::distinct(row, col) |>
  nrow()
