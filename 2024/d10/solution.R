input <- strsplit(readLines(here::here("2024", "d10", "input")), "")
input_matrix <- matrix(as.integer(unlist(input)), nrow = length(input), byrow = TRUE)

directions <- list(
  c(-1, 0),
  c(1, 0),
  c(0, 1),
  c(0, -1)
)

get_value <- function(row, col) {
  if (row < 1 | row > nrow(input_matrix) | col < 1 | col > ncol(input_matrix)) {
    NA
  } else {
    input_matrix[row, col]
  }
}

check_trail <- function(row, col, path = c(), part) {
  point <- get_value(row, col)

  if (point == 9) {
    if (part == 2) {
      return(1)
    } else if (part == 1) {
      return(paste0(row, col))
    }
  }

  adjacent <- purrr::map(
    directions,
    \(x) c(row, col) + x
  )

  adjacent_values <- purrr::map_int(
    adjacent,
    \(x) get_value(x[1], x[2])
  )

  correct_sequence <- which(adjacent_values == point + 1)

  if (length(correct_sequence) == 0) {
    if (part == 1){
    return(NULL)
    } else if (part == 2) {
      return(0)
    }
  } else {
    purrr::map(
      correct_sequence,
      function(i) {
        path = c(path, adjacent[[i]][1], adjacent[[i]][2])
        check_trail(adjacent[[i]][1], adjacent[[i]][2], path, part)
      }
    ) |>
      unlist()
  }
}

positions <- which(input_matrix == 0, arr.ind = TRUE)
  
part1 <- purrr::map(
    seq_len(nrow(positions)),
  \(x) unique(check_trail(positions[x, 1], positions[x, 2], part = 1))
  ) |>
  lengths() |>
  sum()
  
part2 <- purrr::map(
    seq_len(nrow(positions)),
  \(x) check_trail(positions[x, 1], positions[x, 2], part = 2)
  ) |>
  unlist() |>
  sum()

