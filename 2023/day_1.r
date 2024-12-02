input <- readLines(here::here("2023", "data", "day1.txt"))


get_numbers <- function(data) {
  
  parsed <- unlist(
    stringr::str_extract_all(data, "\\d")
  )

  as.integer(
    paste0(
      parsed[c(1, length(parsed))], # Erstes und letztes Element
      collapse = "") # Collapse beide werte zusammen
  )
}

part1 <- sum(purrr::map_dbl(input, get_numbers))

# Part 2
get_numbers_with_words <- function(data) {
  raw <- stringr::str_match_all(
    data, "(?=(\\d|one|two|three|four|five|six|seven|eight|nine))"
  )

  first_match <- raw[[1]][1, 2]
  last_match <- raw[[1]][nrow(raw[[1]]), 2]

  parse_word <- function(word) {
    if (suppressWarnings(!is.na(as.integer(word)))) {
      return(as.integer(word))
    }
    switch(word,
      "one" = 1,
      "two" = 2,
      "three" = 3,
      "four" = 4,
      "five" = 5,
      "six" = 6,
      "seven" = 7,
      "eight" = 8,
      "nine" = 9
    )
  }

  parsed <- unlist(purrr::map(c(first_match, last_match), parse_word))

  return(
    as.integer(
      paste0(
        parsed,
        collapse = ""
      ) # Collapse beide werte zusammen
    )
  )
}

part_2 <- sum(purrr::map_dbl(input, get_numbers_with_words))
