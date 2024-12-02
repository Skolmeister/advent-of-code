tictoc::tic()

input_raw <- readLines(here::here("2023", "data", "day8.txt"))

directions <- unlist(strsplit(input_raw[1], ""))

instructions <- tibble::tibble(input = input_raw[3:length(input_raw)]) |>
  tidyr::separate_wider_delim(
    input, 
    delim = " = ",
    names = c("location", "directions")
  ) |>
  dplyr::mutate(
    directions = gsub("\\(|\\)", "", directions)
  ) |>
  tidyr::separate_wider_regex(
    directions,
    patterns = c(
      left = "\\w{3}",
      ", ",
      right = "\\w{3}"
    )
  )

directions_trans <- rep(dplyr::case_when(
  directions == "L" ~ "left",
  directions == "R" ~ "right"
), 1000000)

get_steps <- function(location, endnode) {
for (dir in seq_along(directions_trans)){
  location <- unlist(instructions[instructions$location == location, directions_trans[dir]])
  if (grepl(endnode, location)) {
    return(dir)
    break
  }
}
}

part_1 <- get_steps("AAA", "ZZZ")

all_nodes_to_check <- instructions$location[grep(".{2}A", instructions$location)]
part_2 <- Reduce(pracma::Lcm, purrr::map_dbl(all_nodes_to_check, get_steps, endnode = ".{2}Z"))

tictoc::toc()


