library(magrittr)

input <- readLines("~/R Projects/adventofcode/2020/Input/2020_d6_input.txt") %>% 
  stringr::str_c(., collapse = "|") %>% 
  stringr::str_split("\\|\\|") %>% 
  unlist()


# Part 1 ------------------------------------------------------------------

count_unique_questions <- function(input){

input %>% 
  stringr::str_remove_all("\\|") %>% 
  stringr::str_split("") %>% 
  unlist() %>% 
  unique() %>% 
  length()
}

result <- purrr::map_dbl(input, count_unique_questions) %>% 
  sum()


# Part 2 -------------------------------------------------------------------
