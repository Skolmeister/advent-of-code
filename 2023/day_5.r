testinput <- readLines(here::here("2023", "data", "day5.txt"))

split_at_position <- function(x, position) {
  unname(split(x, cumsum(seq_along(x) %in% position)))
}

splitted_input <- split_at_position(testinput, grep("map", testinput))

seeds <- splitted_input[[1]][1] |>
  stringr::str_extract_all("\\d+") |>
  unlist() |>
  as.double()

maps <- splitted_input[-1]

  get_sequences <- function(map) {
    Filter(length, 
    lapply(
      stringr::str_extract_all(map, "\\d+"),
    FUN = \(x) as.double(unlist(x))
    )
  )
  }

  all_sequences <- purrr::map(maps, get_sequences)

  find_last_seed <- function(seed, sequences) {
    
  for (seq in seq_along(sequences)) {

      for (map in seq_along(sequences[[seq]])) {

        check_seed <- sequences[[seq]][[map]][2] <= seed && seed < sequences[[seq]][[map]][2] + sequences[[seq]][[map]][3]
        if (check_seed == TRUE) {
          seed <- seed + sequences[[seq]][[map]][1] - sequences[[seq]][[map]][2]
          break
        } else {
          next
        }
      }
    }
    return(seed)
  }

tictoc::tic()
part_1 <- min(purrr::map_dbl(rep(seeds, 10000000), find_last_seed, sequences = all_sequences))
tictoc::toc()