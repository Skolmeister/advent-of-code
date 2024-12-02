input_raw <- strsplit(readLines(here::here("2023", "data", "day10.txt")), "")

allowed_below <- c("|", "L", "J")
allowed_above <- c("|", "7", "F")
allowed_right <- c("-", "7", "J")
allowed_left <- c("-", "L", "F")

get_adjacent <- function(i, j) {
  symbol <- pipe_plan[i, j]
  allowed_directions <- switch(symbol,
    "|" = c("above", "below"),
    "-" = c("left", "right"),
    "L" = c("above", "right"),
    "7" = c("below", "left"),
    "F" = c("below", "right"),
    "J" = c("above", "left"),
    "S" = c("above", "below", "left", "right")
  )

  adjacent <- tibble::tribble(
    ~position, ~row, ~column,
    "above", i - 1, j,
    "right", i, j + 1,
    "below", i + 1, j,
    "left", i, j - 1,
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      value = pipe_plan[row, column],
      allowed = dplyr::case_when(
        position == "above" && value %in% allowed_above ~ TRUE,
        position == "below" && value %in% allowed_below ~ TRUE,
        position == "right" && value %in% allowed_right ~ TRUE,
        position == "left" && value %in% allowed_left ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::filter(
      allowed == TRUE,
      position %in% allowed_directions
    ) |>
    dplyr::ungroup() |>
    head(1)

  pipe_plan[i, j] <<- "x"
  path_tbl <<- path_tbl |>
    dplyr::add_row(
      x = j,
      y = i
    )
  counter <<- counter + 1
  return(adjacent)
}


pipe_plan <- matrix(unlist(input_raw), ncol = length(input_raw[[1]]), byrow = TRUE)
# Expand the matrix to avoid out of bounds
pipe_plan <- rbind(pipe_plan, rep(".", ncol(pipe_plan)))
pipe_plan <- cbind(pipe_plan, rep(".", nrow(pipe_plan)))
pipe_plan <- rbind(rep(".", nrow(pipe_plan)), pipe_plan)
pipe_plan <- cbind(rep(".", nrow(pipe_plan)), pipe_plan)

path_tbl <- tibble::tibble(
  x = integer(),
  y = integer()
)

element_s <- which(pipe_plan == "S", arr.ind = TRUE)

counter <- 0
starting_points <- get_adjacent(element_s[1], element_s[2])

end_value <- "S"
start_value <- starting_points$value

while (nrow(starting_points) > 0) {
  starting_points <- get_adjacent(starting_points$row, starting_points$column)
}

part_1 <- ceiling(counter / 2)

# The polyarea function uses gaussian method to calculate area of polygon
# the area consists of length 1 squares that connect two pipes
# So a polygon without any enclosed dots should have the area "length of path / 2"
# If we want to get the enclosed dots, we simply look for the area - half of the path length
# because the connection to the starting point misses in path_mat, we have to add one at the end
part_2 <- abs(pracma::polyarea(path_tbl$x, path_tbl$y)) - (nrow(path_tbl) / 2) + 1
