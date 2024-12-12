input <- readLines(here::here("2024", "d6", "input"))

input_matrix <- input |>
  strsplit("") |>
  unlist() |>
  matrix(
    nrow = length(input),
    ncol = nchar(input[1]),
    byrow = TRUE
  )

directions <- list(
  c(-1, 0),
  c(0, 1),
  c(1, 0),
  c(0, -1)
)

start <- which(
  input_matrix != "." & input_matrix != "#",
  arr.ind = TRUE
)

check_symbol <- function(pos, mat) {
  mat[pos[1, 1], pos[1, 2]]
}

get_path <- function(point, mat, part, new_obstacle = c(0,0)) {
obstacle_count <- 0
dict <- list()
repeat {
mover <- (obstacle_count %% 4) + 1
prev_position <- point
next_position <- point + unlist(directions[mover])
# print(point)
if (next_position[1] > nrow(mat) || any(next_position == 0) || next_position[2] > ncol(mat)) {
  if (part == 1) {
    return(dict)
  } else {
    return(0)
  }
}

if (check_symbol(next_position, mat) == "#" || all(next_position == new_obstacle)) {
  obstacle_count <- obstacle_count + 1
} else {
  point <- next_position
  point_dict <- paste0(point[1], " ", point[2])
  if (part == 2) {
    if (!is.null(dict[[point_dict]]) && directions[mover] %in% dict[[point_dict]]) {
      # print("loop detected")
      return(1)
    }
  }
  dict[[point_dict]][length(dict[[point_dict]]) + 1] <- directions[mover]
  }
}
}

get_loop <- function(start_point, new_obstacle_position, mat) {
  mat[new_obstacle_position[1], new_obstacle_position[2]] <- "#"
  print(new_obstacle_position)
  get_path(start_point, mat, part = 2)
}


result <- get_path(start, input_matrix, part = 1)
part1 <- length(result)

visited_points <- purrr::map(strsplit(names(result), " "), as.integer)
# part2 <- sum(unlist(purrr::map(visited_points, \(x) get_loop(start, x, input_matrix))))

part2 <- purrr::map_int(
  visited_points,
  \(x) get_path(start, input_matrix, part = 2, x) 
) |>
sum()
