library(future)
plan(multisession)
input <- readLines(here::here("2024", "d6", "input"))

input_matrix <- input |>
  strsplit("") |>
  unlist() |>
  matrix(
    nrow = length(input),
    ncol = nchar(input[1]),
    byrow = TRUE
  )

input_matrix <- rbind(NA, cbind(NA, input_matrix, NA), NA)

get_direction <- function(mat, point) {
  dir <- mat[point]
  index <- dplyr::case_when(
    dir == "^" ~ c(-1, 0),
    dir == ">" ~ c(0, 1),
    dir == "<" ~ c(0, -1),
    dir == "V" ~ c(1, 0)
  )
  # index_to_move <- point + index

  return(index)
}

change_direction <- function(symbol) {
  dplyr::case_when(
    symbol == "^" ~ ">",
    symbol == ">" ~ "V",
    symbol == "V" ~ "<",
    symbol == "<" ~ "^"
  )
}

check_symbol <- function(mat, pos) {
  mat[pos[1, 1], pos[1, 2]]
}



count_positions <- function(mat, start_point, type) {
  direction <- get_direction(mat, start_point)
  symbol_start <- check_symbol(mat, start_point)
  position <- start_point
  current_symbol <- check_symbol(mat, start_point)
  move_index <- list(start_point)
  # while (!is.na(check_symbol(mat, position))) {
    repeat {
    prev_symbol <- check_symbol(mat, position)
    position <- position + direction
    current_symbol <- check_symbol(mat, position)
    if (is.na(current_symbol)) {
      if (type == 1) {
        break
      } else if (type == 2) {
        return()
      }
    } else if (current_symbol != "#") {
      mat_old <- mat
      move_index <- append(list(position), move_index)
      mat[position[1, 1], position[1, 2]] <- prev_symbol
      
      loop_match <- all((mat_old == mat) == TRUE, na.rm = TRUE)
      # print(mat[position[1, 1], position[1, 2]] == prev_symbol)
    } else if (!is.na(current_symbol)) {
      new_symbol <- change_direction(
        check_symbol(
          mat,
          pos = matrix(move_index[[1]][1, ], ncol = 2)
        )
      )
      prev_position <- position - direction
      mat_old <- mat
      mat[prev_position[1, 1], prev_position[1, 2]] <- new_symbol
      loop_match <- all((mat_old == mat) == TRUE, na.rm = TRUE)
      direction <- get_direction(mat, prev_position)
      position <- prev_position
    }

    if (loop_match) {
      break
    }

    # if (all(position == start_point)) {
    #   symbol_start <- c(symbol_start, check_symbol(mat, start_point))
    #   print(symbol_start)
    #   if (any(table(symbol_start) > 1)) {
    #     break
    #   }
    # }
    
  }

  return(
    list(
      mat,
      length(which(mat %in% c("^", "V", ">", "<"))),
      move_index
    )
  )
}

start_point <- which(
  input_matrix != "." & input_matrix != "#",
  arr.ind = TRUE
)

part1 <- count_positions(input_matrix, start_point, type = 1) 

positions <- part1[[3]]

positions_unique <- purrr::reduce(positions, rbind) |>
  unique() |>
  tibble::as_tibble() |>
  tibble::rownames_to_column() |>
  dplyr::group_by(rowname) |>
  dplyr::group_split() |>
  purrr::map(
    \(x) matrix(dplyr::select(x, -rowname), ncol = 2)
  )

construct_obstacle <- function(mat, position, start_point) {
  mat[unlist(position[1,1]), unlist(position[1,2])] <- "#"
  count_positions(mat, start_point, type = 2)
}

part2 <- furrr::future_map(
  positions_unique,
  \(x) construct_obstacle(
    mat = input_matrix,
    position = x,
    start_point = start_point
  )[[1]],
  .progress = TRUE
)

length(part2[lengths(part2) != 0])

