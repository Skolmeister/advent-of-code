testinput <- c(
  "467..114..",
  "...*......",
  "..35..633.",
  "......#...",
  "617*......",
  ".....+.58.",
  "..592.....",
  "......755.",
  "...$.*....",
  ".664.598.."
)

input <- readLines(here::here("2023", "data", "day3.txt"))


splitted_input <- strsplit(input, split = "")

linelength <- length(splitted_input[[1]])

input_matrix <- matrix(unlist(splitted_input), ncol = linelength, byrow = TRUE)

input_matrix <- rbind(input_matrix, rep(".", ncol(input_matrix)))
input_matrix <- cbind(input_matrix, rep(".", nrow(input_matrix)))
result_tibble <- tibble::tibble(
  row = integer(),
  column = integer(),
  number = integer()
)

for (i in seq_len(nrow(input_matrix))) {
  
  for (j in seq_along(input_matrix[i, ])) {
    if (!stringr::str_detect(input_matrix[i, j], "\\d")) {
      next
    }
    print(c(i, j))
    # if (i == nrow(input_matrix)) {
    #   adjacent <- c(
    #     above = input_matrix[i - 1, j],
    #     diag_above_right = input_matrix[i - 1, j + 1],
    #     right = input_matrix[i, j + 1],
    #     left = input_matrix[i, j - 1],
    #     diag_above_left = input_matrix[i - 1, j - 1]
    #   )
    # } else if (j == ncol(input_matrix)) {
    #   adjacent <- c(
    #     above = input_matrix[i - 1, j],
    #     below = input_matrix[i + 1, j],
    #     diag_below_left = input_matrix[i + 1, j - 1],
    #     left = input_matrix[i, j - 1],
    #     diag_above_left = input_matrix[i - 1, j - 1]
    #   )
    # } else {
      adjacent <- c(
        above = input_matrix[i - 1, j],
        diag_above_right = input_matrix[i - 1, j + 1],
        right = input_matrix[i, j + 1],
        diag_below_right = input_matrix[i + 1, j + 1],
        below = input_matrix[i + 1, j],
        diag_below_left = input_matrix[i + 1, j - 1],
        left = input_matrix[i, j - 1],
        diag_above_left = input_matrix[i - 1, j - 1]
      )
    # }
    
    if (any(stringr::str_detect(adjacent, "[^\\.\\d]"))) { # Symbol detected
      dots <- which(stringr::str_detect(input_matrix[i, ], "\\D"))
      
      next_dot <- dots[which(dots == min(dots[dots > j]))]
      prev_dot <- dots[which(dots == max(dots[dots < j]))]

      if (length(prev_dot) == 0) {
        numbers <- as.integer(
          paste0(
            input_matrix[i, 1:(next_dot - 1)], 
            collapse = "")
            )
      } else if (length(next_dot) == 0) {
        numbers <- as.integer(
          paste0(
            input_matrix[i, (prev_dot + 1):length(input_matrix[i, ])], 
            collapse = "")
            )
      } else {
        numbers <- as.integer(
          paste0(
            input_matrix[i, (prev_dot + 1) : (next_dot - 1)], 
            collapse = "")
            )
      }

      result <- tibble::tibble(
        row = i,
        column = j,
        number = numbers
      )

      result_tibble <- result_tibble |>
        dplyr::add_row(result)

    } else {
      next
    }


  }
  
}


part_1 <- result_tibble |> 
  dplyr::group_by(row) |>
  # dplyr::add_count(number) |>
  # dplyr::filter(n > 1) |>
  # dplyr::arrange(desc(n)) |>
  dplyr::group_by(row, number) |>
  dplyr::mutate(
    occurence = column != dplyr::lag(column) + 1
  ) |>
  tidyr::replace_na(
    list(
      occurence = TRUE
    )
  ) |>
  dplyr::mutate(
    occurence = cumsum(occurence)
  ) |>
  dplyr::distinct(row, number, occurence, .keep_all = TRUE) |>
  dplyr::pull(number) |>
  sum()


gear_tibble <- tibble::tibble(
  row = integer(),
  column = integer(),
  value = character(),
  number = integer(),
  row_number = integer()
)


for (i in seq_len(nrow(input_matrix))) {
  
  for (j in seq_along(input_matrix[i, ])) {
    
    if (!stringr::str_detect(input_matrix[i, j], "\\d")) {
      next
    }

      adjacent <- tibble::tribble(
        ~position, ~row, ~column,
        "above", i - 1, j,
        "diag_above_right", i - 1, j + 1,
        "right", i, j + 1,
        "diag_below_right", i + 1, j + 1,
        "below", i + 1, j,
        "diag_below_left", i + 1, j - 1,
        "left", i, j - 1,
        "diag_above_left", i - 1, j - 1,
      ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          value = list(
            dplyr::case_when(
              length(input_matrix[row, column]) > 0 ~ input_matrix[row, column],
              TRUE ~ NA_character_
            )
          )
        ) |>
        tidyr::unnest_longer(value)


    if (any(stringr::str_detect(adjacent$value, "\\*"))) { # Gear detected
      dots <- which(stringr::str_detect(input_matrix[i, ], "\\D"))
      
      next_dot <- dots[which(dots == min(dots[dots > j]))]
      prev_dot <- dots[which(dots == max(dots[dots < j]))]

      if (length(prev_dot) == 0) {
        numbers <- as.integer(
          paste0(
            input_matrix[i, 1:(next_dot - 1)], 
            collapse = "")
            )
      } else if (length(next_dot) == 0) {
        numbers <- as.integer(
          paste0(
            input_matrix[i, (prev_dot + 1):length(input_matrix[i, ])], 
            collapse = "")
            )
      } else {
        numbers <- as.integer(
          paste0(
            input_matrix[i, (prev_dot + 1) : (next_dot - 1)], 
            collapse = "")
            )
      }

      gear <- adjacent |>
        dplyr::filter(
          value == "*"
        ) |>
        dplyr::mutate(
          number = numbers,
          row_number = i
        ) |>
        dplyr::select(-position)

      if (nrow(gear) == 2) {
        print(c(i, j))
        break
      }
      
      result <- tibble::tibble(
        row = i,
        column = j,
        number = numbers
      )

      gear_tibble <- gear_tibble |>
        dplyr::add_row(gear)

      result_tibble <- result_tibble |>
        dplyr::add_row(result)

    } else {
      next
    }


  }
  
}

gear_tibble |>
  dplyr::group_by(row, column) |>
  dplyr::distinct(row, column, number, row_number) |>
  dplyr::add_count() |> 
  dplyr::filter(n == 2) |>
  dplyr::mutate(
    position = glue::glue("number_{dplyr::row_number()}")
  ) |>
  dplyr::arrange(row, column) |>
  dplyr::select(row, column, number, position) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    names_from = position,
    values_from = number
  )  |> 
  dplyr::mutate(
    gear_ratio = number_1 * number_2
  ) |> 
  dplyr::pull(gear_ratio) |>
  sum()

