# Day 6
# Input -------------------------------------------------------------------
input_path <- "~/R Projects/adventofcode/2021/Input/2021_d6_input"
input <- as.integer(unlist(strsplit(readLines(input_path), ",")))

# Part 1 ------------------------------------------------------------------

#Model the growth of an Lanternfish school

reset_timer <- 6
new_fish_timer <- 8

part1_duration <- 80
part2_duration <- 256

for(i in 1:part2_duration){
  if(i == 1){prev_day <- vctrs::vec_count(input)}

  counts_reduced <- prev_day %>% 
    dplyr::arrange(key) %>% 
    dplyr::mutate(key = key - 1) 
  
  new_population <- counts_reduced %>% 
    dplyr::add_row(key = 8, 
                   count = counts_reduced[counts_reduced$key == -1 ,2, drop = TRUE]) %>% 
    dplyr::mutate(key = dplyr::case_when(
      key == -1 ~ 6,
      TRUE ~ key
    )) %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarize(count = sum(count))
  
  prev_day <- new_population
  
  result <- sum(prev_day$count)
  
}

print(result)
