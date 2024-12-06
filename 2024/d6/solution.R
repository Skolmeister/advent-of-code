library(future)
plan(sequential)
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

get_direction <- function(mat, point, reverse) {
  dir <- mat[point]
  index <- dplyr::case_when(
    dir == "^" ~ c(-1, 0),
    dir == ">" ~ c(0, 1),
    dir == "V" ~ c(1, 0),
    dir == "<" ~ c(0, -1)
  )
  return(index)
}

change_direction <- function(dir) {
dplyr::case_when(
    identical(dir, c(-1, 0)) ~ c(0, 1),
    identical(dir, c(0, 1))  ~ c(1, 0),
    identical(dir, c(1, 0))  ~ c(0, -1),
    identical(dir, c(0, -1)) ~ c(-1, 0)
  )
}

change_direction_symbol <- function(symbol) {
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

start_point <- which(
  input_matrix != "." & input_matrix != "#",
  arr.ind = TRUE
)

point <- start_point
test <- function(i_mat, point, part) {
  dir <- get_direction(check_symbol(i_mat, point))
  mat <- i_mat
  repeat {
    next_symbol <- mat[point + dir]
    if (is.na(next_symbol)) {
      break
    } else if (next_symbol == "#") {
      old_dir <- dir
      dir <- change_direction(dir)
      check_next_point <- check_symbol(mat, point + dir)
      if (check_next_point != "#") {
      sym <- change_direction_symbol(check_symbol(mat, point))
      } else {
        while (check_next_point == "#") {
        sym <- change_direction_symbol(
          change_direction_symbol(
          check_symbol(
            mat, point
            )
          )
        )
      dir <- change_direction(change_direction(dir))
      check_next_point <- check_symbol(mat, point + dir)
      mat[point] <- sym
      break
        }
      }
    } else {
      sym <- check_symbol(mat, point)
    }
    point <- point + dir
    mat_prev <- mat
    mat[point] <- sym
    if (part == 2) {
      if (all((mat_prev == mat) == TRUE, na.rm = TRUE)) {
        loop <- TRUE
        break
      } else {
        loop <- FALSE
      }
    }
  }
  if (part == 1) {
    return(mat)
  } else {
    return(loop)
  }
}

tictoc::tic()
res <- test(input_matrix, point = start_point, part = 1)
tictoc::toc()
positions <- which(res != "." & res != "#" & !is.na(res), arr.ind = TRUE)
part1 <- nrow(positions)

mat <- input_matrix

construct_obstacle <- function(mat, position, start_point) {
  mat[unlist(position[1]), unlist(position[2])] <- "#"
  test(i_mat = mat, point = start_point, part = 2)
}


loops <- 0
count <- c()
for (i in seq_len(nrow(positions))) {
  print(i)
  if (identical(c(positions[i,], use.names = FALSE), c(start_point))) {
    next
  }
  res <- construct_obstacle(mat = input_matrix, position = positions[i, ], start_point)
  loops <- loops + res
}
loops

get_loop <- function(mat, positions, index, start_point) {
  if (identical(c(positions[index, ], use.names = FALSE), c(start_point))) {
    return()
  }

  construct_obstacle(
    mat = mat,
    position = positions[index, ],
    start_point
  )

}

tictoc::tic()
furrr::future_map_lgl(
  seq_len(part1),
  \(x) get_loop(
    input_matrix,
    positions,
    x,
    start_point
  ),
  .progress = TRUE
)
tictoc::toc()
part1 <- length(which(mat != "." & mat != "#" & !is.na(mat)))
tictoc::toc()

ind <- unique(which(mat == "X", arr.ind = TRUE))


