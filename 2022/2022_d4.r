# Advent of Code 2022 Day 4

input <- readr::read_csv(here::here("2022", "input", "2022_d4_input.txt"),
    col_names = c("elf_1", "elf_2") # Read input as csv
) |>
    dplyr::rowwise() |>
    dplyr::mutate(sections1 = list(seq( # Make sequences from the sections
        as.integer(stringr::str_extract(elf_1, "^.+(?=-)")),
        as.integer(stringr::str_extract(elf_1, "(?<=-).+$"))
    )),
    sections2 = list(seq(
        as.integer(stringr::str_extract(elf_2, "^.+(?=-)")),
        as.integer(stringr::str_extract(elf_2, "(?<=-).+$")))))  

# Part 1    

intersections <- input |> # Just check if one contains the other completely
    dplyr::mutate(one_contains_the_other = dplyr::case_when( 
        all(sections1 %in% sections2) ~ TRUE,
        all(sections2 %in% sections1) ~ TRUE,
        TRUE ~ FALSE
    ))

sum(intersections$one_contains_the_other)

# Part 2

overlaps <- input |> # Change all() to any()
    dplyr::mutate(one_contains_the_other = dplyr::case_when(
        any(sections1 %in% sections2) ~ TRUE,
        any(sections2 %in% sections1) ~ TRUE,
        TRUE ~ FALSE
    ))
    
sum(overlaps$one_contains_the_other)
