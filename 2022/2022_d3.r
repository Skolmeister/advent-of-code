

input <- readr::read_lines(here::here("2022", "input", "2022_d3_input.txt"))


analyze_compartment <- function(string) {

first_compartment <- substring(string, 1, nchar(string) / 2)
second_compartment <- substring(string, (nchar(string) / 2 + 1), nchar(string))

a <- unlist(strsplit(first_compartment, ""))
b <- unlist(strsplit(second_compartment, ""))

in_both <- unique(b[pmatch(a,b)[!is.na(pmatch(a,b))]])

get_score <- function(letter){

if(letter %in% letters) {
    score <- which(letter == letters)
} else if (letter %in% LETTERS) {
    score <- which(letter == LETTERS) + length(letters)
}
return(score)
}

    score_total <- unlist(purrr::map(in_both, get_score))

    return(score_total)

}

sum(unlist(purrr::map(input, analyze_compartment)))


groups <- input |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(group = ceiling(dplyr::row_number()/3)) |> 
    dplyr::group_split(group)

find_badge <- function(groups){

split_groups <- purrr::map(groups$value, function(x){unlist(strsplit(x, split = ""))})

matches_first <- pmatch(split_groups[[1]], split_groups[[2]])[!is.na(pmatch(split_groups[[1]], split_groups[[2]]))]
in_first_two <- unique(split_groups[[2]][matches_first])

matches_second <- pmatch(in_first_two, split_groups[[3]])[!is.na(pmatch(in_first_two, split_groups[[3]]))]
in_all <- unique(split_groups[[3]][matches_second])

get_score <- function(letter){

if(letter %in% letters) {
    score <- which(letter == letters)
} else if (letter %in% LETTERS) {
    score <- which(letter == LETTERS) + length(letters)
}
return(score)
}

get_score(in_all)
}



sum(unlist(purrr::map(groups, find_badge)))
