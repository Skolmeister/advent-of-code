input <- readLines(here::here(
  "2024", "d4", "input"
)) 

input_matrix <- input |>
strsplit("") |>
unlist() |>
matrix(
  nrow = length(input),
  ncol = nchar(input[1]),
  byrow = TRUE
  )

input_matrix <- rbind(NA, NA, NA, cbind(NA, NA, NA, input_matrix, NA, NA, NA), NA, NA, NA)

pos_x <- which(input_matrix == "X", arr.ind = TRUE)

directions <- tibble::tibble(
  row_shift = c(rep(-1,3), rep(0, 2), rep(1, 3)),
  col_shift = c(seq(-1,1), -1, 1, seq(-1,1)) 
)

target_word <- "XMAS"

all_words_p1 <- tibble::tibble(
  word = character(),
  row = integer(),
  col = integer()
)

for (i in seq_len(nrow(pos_x))) {
  row <- pos_x[i, "row"]
  col <- pos_x[i, "col"]
  result <- directions |>
    dplyr::rowwise() |>
    dplyr::mutate(
      letter_1 = input_matrix[row, col],
      letter_2 = list(input_matrix[row + row_shift, col + col_shift]),
      letter_3 = list(input_matrix[row + (row_shift * 2), col + (col_shift * 2)]),
      letter_4 = list(input_matrix[row + (row_shift * 3), col + (col_shift * 3)]),
      word = paste0(letter_1, letter_2, letter_3, letter_4, collapse = "")
    ) |>
    dplyr::filter(
      word == target_word
    ) |>
    dplyr::select(word) |>
    dplyr::mutate(
      row = row,
      col = col
    ) 

  all_words_p1 <- rbind(all_words_p1, result)
}

all_words_p1

directions_cross <- tibble::tibble(
  row_shift = c(-1, -1, 1, 1),
  col_shift = c(-1, 1, -1, 1) 
)

pos_ms <- which(input_matrix == "M" | input_matrix == "S", arr.ind = TRUE)
target_word <- "MAS"

all_words_p2 <- tibble::tibble(
  word = character(),
  index_a = integer(),
  row = integer(),
  col = integer()
)

for (i in seq_len(nrow(pos_ms))) {
  row <- pos_ms[i, "row"]
  col <- pos_ms[i, "col"]
  result <- directions_cross |>
    dplyr::rowwise() |>
    dplyr::mutate(
      letter_1 = input_matrix[row, col],
      letter_2 = input_matrix[row + row_shift, col + col_shift],
      letter_3 = input_matrix[row + (row_shift * 2), col + (col_shift * 2)],
      index_a = list(c(row + row_shift, col + col_shift)), 
      word = paste0(letter_1, letter_2, letter_3, collapse = "")
    ) |>
    dplyr::filter(
      word == target_word
    ) |>
    dplyr::select(word, index_a) |>
    dplyr::mutate(
      row = row,
      col = col
    ) 

  all_words_p2 <- rbind(all_words_p2, result)
}

p2 <- all_words_p2 |>
  dplyr::count(index_a) |>
  dplyr::filter(n == 2) |>
  nrow()
