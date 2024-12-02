library(magrittr)

input <- readLines("~/R Projects/adventofcode/2020_d2_input.txt")

nicer_input <- input %>% 
  stringr::str_split(., "\\s|-") %>% 
  tibble::as_tibble_col() %>%
  tidyr::unnest_wider(col = value) %>% 
  dplyr::rename("min" = `...1`,
                "max" = `...2`,
                "letter" = `...3`,
                "password" = `...4`) %>% 
  dplyr::mutate(letter = stringr::str_remove_all(letter, ":")) %>% 
  dplyr::mutate_at(dplyr::vars(c(1,2)), as.integer)



# Part 1 ------------------------------------------------------------------

result <- nicer_input %>%
  dplyr::mutate(letter_count = stringr::str_count(password, letter),
                within_policy = dplyr::case_when(
                                                 letter_count >= min & letter_count <= max ~ "TRUE",
                                                 TRUE ~ "FALSE")) %>% 
  dplyr::filter(within_policy == TRUE) %>% 
  dplyr::count(within_policy) %>% 
  dplyr::pull(n)


# Part 2 ------------------------------------------------------------------

correct_position <- function(password, letter, position1, position2){
  
  position_of_char <- stringr::str_locate_all(password, letter) %>% 
    unlist() %>% 
    unique()
  
  if((position1 %in% position_of_char & !position2 %in% position_of_char) |
     (!position1 %in% position_of_char & position2 %in% position_of_char)){
    result <- TRUE
  } else {
    result <- FALSE
  }
  
  return(result)
}

result2 <- nicer_input %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(within_new_policy = correct_position(password, letter, min, max)) %>% 
  dplyr::filter(within_new_policy == TRUE) %>% 
  dplyr::count(within_new_policy) %>% 
  dplyr::pull(n)
