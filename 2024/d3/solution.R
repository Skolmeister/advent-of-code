tictoc::tic()
input <- paste0(readLines(here::here("2024", "d3", "input")), collapse = "")
regex <- "mul\\(\\d{1,3}\\,\\d{1,3}\\)"

part1 <- unlist(
  stringr::str_extract_all(
    input,
    regex
  )
) |>
  stringr::str_extract_all(
    "\\d{1,3}"
  ) |>
  purrr::map(
    \(x) prod(as.integer(x))
  ) |>
  unlist() |>
  sum()

regex_p2 <- "(mul\\(\\d{1,3}\\,\\d{1,3}\\)|do(n't)?\\(\\))"
instructions <- unlist(
  stringr::str_extract_all(
    input, regex_p2
  )
)

switch <- 1
part2 <- 0
for (i in seq_len(length(instructions))) {
  if (instructions[i] == "do()") {
    switch <- 1
  }
  if (instructions[i] == "don't()") {
    switch <- 0
  }
  if (stringr::str_detect(instructions[i], "\\d{1,3}")) {
    result_multiplication <- instructions[i] |>
      stringr::str_extract_all(
        "\\d{1,3}"
      ) |>
      unlist() |>
      as.integer() |>
      prod() |>
      prod(switch)
    part2 <- sum(part2, result_multiplication)
  }
}

part2
tictoc::toc()
