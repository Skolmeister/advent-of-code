library(magrittr)

input_path <- "~/R Projects/adventofcode/2021/Input/2021_d1_input"

input <- as.integer(readLines(input_path))

# Part 1 ------------------------------------------------------------------

get_in_or_decrease <- function(input) {

  result <- dplyr::as_tibble(input) %>% 
  dplyr::mutate(previous_depth = dplyr::lag(value), #Take previous value
                in_or_decrease = dplyr::case_when(
                  value < previous_depth ~ "decrease", # Check if in- or decrease
                  value > previous_depth ~ "increase",
                  TRUE ~ "no change"
                  )
                ) %>% 
  dplyr::count(in_or_decrease) # Count them
  
  return(result)
}

get_in_or_decrease(input)

# Part 2 ------------------------------------------------------------------

#First calculate the new 3-measure rolling window 
#(could surely use something like Rcpp::roll_sum, too)
new_depth <- function(measurement_no) {
  sum(input[measurement_no:(measurement_no+2)])
}

new_measures <- purrr::map_dbl(1:(length(input-2)), new_depth) 

solution2 <- get_in_or_decrease(new_measures) #reuse the first function
