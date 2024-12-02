# Advent of Code Day 4 --- Secure Container ---

# Part 1
library(tibble)
library(dplyr)

input <- c(125730:579381)

# Separate Input in single digits
input_table <- tibble(p1 = substr(input, 1, 1), 
                      p2 = substr(input, 2, 2),
                      p3 = substr(input, 3, 3),
                      p4 = substr(input, 4, 4),
                      p5 = substr(input, 5, 5),
                      p6 = substr(input, 6, 6))

pw_combinations <- input_table %>% 
  filter(p1 <= p2 & p2 <= p3 & p3 <= p4 & p4 <= p5 & p5 <= p6,
         p1 == p2 | p2 == p3 | p3 == p4 | p4 == p5 | p5 == p6)

par1 <- nrow(pw_combinations)

# Part 2
library(purrr)

# Get the digits back together
pw_combined <- pw_combinations %>% 
  mutate(pw = paste(p1,p2,p3,p4,p5,p6,sep = ""),
         pw = as.numeric(pw)) %>% 
  pull(pw)

# Convert to numeric list (with str_split it gets a list of character vectors)
digits <- function(number, units){ number %/% units %% 10}

pw_as_list <- map(pw_combined, digits, units = 10^c(5:0))


# Function to find strings with the occurence of exact 2 adjacent numbers. Function by @AdamGruer (Twitter), https://github.com/adam-gruer (Github)
has_adjacent <- function(x){
  
  (rle(x)$lengths == 2) %>% any()
  
}

part2 <- map_lgl(pw_as_list, has_adjacent) %>% 
  sum()
