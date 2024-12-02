# Inventory split by newline
input <- tibble::tibble(
    calories = as.integer(
        readLines(
            here::here("2022", "input", "2022_d1_input.txt")
        )
    )
)

# Parse the Input
parsed_input <- input |>
    dplyr::group_by(elf = cumsum(is.na(calories)) + 1) |> # Count NAs with cumsum and use this as groups
    dplyr::summarise(calories_per_elf = sum(calories, na.rm = T), .groups = "drop")

# Part 1
# Just the maximum
parsed_input |>
    dplyr::slice_max(calories_per_elf, n = 1)

# Part 2
# Sum of Max 3
parsed_input |>
    dplyr::slice_max(calories_per_elf, n = 3) |>
    dplyr::summarise(sum_calories = sum(calories_per_elf))
