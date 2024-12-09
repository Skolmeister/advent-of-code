input <- readLines(
  here::here("2024", "d7", "input")
) |>
  strsplit(":\\s")

check_result <- function(value_list) {
  
  needed_result <- as.double(value_list[1])

  values <- as.double(unlist(strsplit(value_list[2], "\\s")))

  full_sum <- sum(values)
  full_product <- prod(values)

  if (any(c(full_sum, full_product) == needed_result)) {
    return(needed_result)
  }

  intermed_result <- rep(values[1], 2)
  for (v in seq_len(length(values) - 1)) {
    res_prod <- purrr::map_dbl(intermed_result, \(x) prod(x, values[v + 1]))
    res_sum <- purrr::map_dbl(intermed_result, \(x) sum(x, values[v + 1]))
    intermed_result <- c(res_prod, res_sum)
  }

  if (any(intermed_result == needed_result)) {
    return(needed_result)
  } else {
    return(0)
  }
}

value_list <- input[[530]]

part1 <- purrr::map_dbl(
  input, check_result
) |>
sum()

check_result_p2 <- function(value_list) {
  
  needed_result <- as.double(value_list[1])

  values <- as.double(unlist(strsplit(value_list[2], "\\s")))

  full_sum <- sum(values)
  full_product <- prod(values)
  full_concat <- as.double(paste0(values, collapse = ""))

  if (any(c(full_sum, full_product, full_concat) == needed_result)) {
    return(needed_result)
  }

  intermed_result <- rep(values[1], 3)
  for (v in seq_len(length(values) - 1)) {
    res_prod <- purrr::map_dbl(intermed_result, \(x) prod(x, values[v + 1]))
    res_sum <- purrr::map_dbl(intermed_result, \(x) sum(x, values[v + 1]))
    res_concat <- purrr::map_dbl(intermed_result, \(x) as.double(paste0(x, values[v + 1], collapse = "")))
    intermed_result <- c(res_prod, res_sum, res_concat)

    if (any(intermed_result > needed_result)) {
      intermed_result <- intermed_result[-which(intermed_result > needed_result)]
    }
  }

  if (any(intermed_result == needed_result, na.rm = TRUE)) {
    return(needed_result)
  } else {
    return(0)
  }
}

part2 <- purrr::map_dbl(
  input, check_result_p2
) |>
sum()
