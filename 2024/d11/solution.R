input <- readLines(here::here("2024", "d11", "input"))
individual_stones <- unlist(strsplit(input, "\\s"))

# Memoise is great here, never used it before but it's really straightforward
# I tried to implement my own memoisation first, this didn't end too well...
# You could run the function without memoise, but it then would have to recalculate each result
# If it's memoized, every function call with its result is stored in cache
# It can then be reused if exactly the same function is called

compute_value <- memoise::memoise(
  function(value, iterations) {
    if (iterations == 0) {
      return(1)
    }
    if (value == "0") {
      compute_value("1", iterations - 1)
    } else if (nchar(value) %% 2 == 0) {
      splitted_stone <- strsplit(value, "")[[1]]
      half_1 <- as.character(as.integer(paste0(splitted_stone[1:(length(splitted_stone) / 2)], collapse = "")))
      half_2 <- as.character(as.integer(paste0(splitted_stone[(length(splitted_stone) / 2 + 1):length(splitted_stone)], collapse = "")))
      compute_value(half_1, iterations - 1) + compute_value(half_2, iterations - 1)
    } else {
      compute_value(as.character(as.double(value) * 2024), iterations - 1)
    }
    # return(stone_value)
  }
)

p1 <- sum(purrr::map_dbl(individual_stones, \(x) compute_value(x, 25)))
p2 <- sum(purrr::map_dbl(individual_stones, \(x) compute_value(x, 75)))

