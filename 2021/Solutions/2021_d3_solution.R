library(magrittr)

input <- readLines(stringr::str_c(here::here(), "/2021/Input/day-03-input.txt"))                 

# Part 1 ------------------------------------------------------------------

gamma_bit <- stringr::str_split(input, "") %>% #Split the input for further processing
  do.call(rbind, .) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.integer(.))) %>%
  dplyr::summarise_all( ~ sum(.)) %>% 
  dplyr::mutate_all( ~ dplyr::case_when(
    . > length(input) / 2 ~ 1, #If sum of 1s is more than half of total, its more 1 than 0
    TRUE ~ 0)
  ) %>% 
  dplyr::mutate(type = "gamma")

epsilon_bit <- gamma_bit %>% 
  dplyr::mutate(dplyr::across(c(V1:V12), ~ ifelse(. == 1, 0, 1))) %>% #epsilon is like gamma but with vice-versa logic
  dplyr::mutate(type = "epsilon")

converted <- dplyr::bind_rows(gamma_bit, epsilon_bit) %>%
  tidyr::pivot_longer(cols = c(1:12),
                      names_to = "position",
                      values_to = "value") %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(bit = stringr::str_c(value, collapse = "")) %>% #Concatenate values to one new bit
  dplyr::mutate(integer = bit64::as.integer64.bitstring(bit)) #Convert to integer
  
power_consumption <- prod(converted$integer) #Solution - Yay, first star!

# Part 2 ------------------------------------------------------------------

# OXYGEN GENERATOR

oxygen_generator_rating <- input #Setup initial vector of values

for(i in 1:stringr::str_length(oxygen_generator_rating[1])){ #Loop through each position in bit

t <- table(substring(oxygen_generator_rating, i,i)) #Get count of 0 and 1 in substring
filter_criteria <-  names(t[t==max(t)]) #Take the higher one

if(length(filter_criteria) == 2){
  filter_criteria <- "1" #If both counts are equal, take 1
}

oxygen_generator_rating <- oxygen_generator_rating[substring(oxygen_generator_rating,i,i) == filter_criteria] #Reduce the original vector to its new size

}

oxygen_generator_rating <- bit64::as.integer64.bitstring(oxygen_generator_rating) #Convert to integer again

#####################################

#CO2 SCRUBBER

co2_scrubber_rating <- input

#Same logic like above, just min instead of max for filter 
for(i in 1:stringr::str_length(co2_scrubber_rating[1])){
  
  t <- table(substring(co2_scrubber_rating, i,i))
  filter_criteria <-  names(t[t==min(t)]) 
  
  if(length(filter_criteria) == 2){
    filter_criteria <- "0"
  }
  
  co2_scrubber_rating <- co2_scrubber_rating[substring(co2_scrubber_rating,i,i) == filter_criteria]
  
}


co2_scrubber_rating <- bit64::as.integer64.bitstring(co2_scrubber_rating)


life_support_rating <- oxygen_generator_rating * co2_scrubber_rating #Solution - Yay, second star



