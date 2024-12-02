library(magrittr)

input <- readLines("~/R Projects/adventofcode/2020/Input/2020_d3_input.txt") #Read Input


# Part 1 ------------------------------------------------------------------


for(i in 1:length(input)){ 
  
  row <- input[i] #iterate through each row 
  position <- 1 + ((i-1) * 3) #Count trees at this position
  
if(position > stringr::str_count(row)){ #needed if pattern exceeds to the right
  repeat{
    if(!exists("x")){x <<- 1}
    
    row <- stringr::str_c(rep(row, x +1), collapse = "") #Paste together as many patterns as needed 
    x <<- x + 1
    
    if(position <= stringr::str_count(row)){
      break #stop if pattern long enough
    }
    
    } 
}

trees <- stringr::str_locate_all(row, "\\#") %>% #find location of trees
  unlist() %>% #str_locate_all makes a list, therefore unlist and throw out duplicates
  unique()

if(position %in% trees){ 
  if(!exists("result")){
    result <<- 1 #initialize count 
    #print(stringr::str_c("Tree here at position ", position))
    } else {
    result <<- result + 1 #count every time a tree occurs
    #print(stringr::str_c("Tree here at position ", position))
    }
} 
  
}


# Part 2 ------------------------------------------------------------------

#Build functions for slopes
first_slope <- function(x){
  x <- x
  y <- x
  return(tibble::tibble(x,y))}
second_slope <- function(x){
  y <- x
  x <- 1 + ((x-1) * 3)
  return(tibble::tibble(x,y))}
third_slope <- function(x){
  y <- x
  x <- 1 + ((x-1) * 5)
  return(tibble::tibble(x,y))}
fourth_slope <- function(x){
  y <- x
  x <- 1 + ((x-1) * 7)
  return(tibble::tibble(x,y))}
fifth_slope <- function(x){
  x <- x
  y <- 1 + (x-1) * 2
  return(tibble::tibble(x,y))
  }

# Combine slopes as list
input_tibbles <- list(purrr::map_df(c(1:length(input)), first_slope),
                      purrr::map_df(c(1:length(input)), second_slope),
                      purrr::map_df(c(1:length(input)), third_slope),
                      purrr::map_df(c(1:length(input)), fourth_slope),
                      purrr::map_df(c(1:length(input)), fifth_slope))


slopes <- function(position_tibble, input){

  position_vector <- dplyr::pull(position_tibble, x) #Vector of Position within input row (horizontal)
  row_vector <- dplyr::pull(position_tibble, y) %>%  #Vector of input rows (vertical)
    .[.<=length(input)] #Needed because fifth slope is not as long as input
  
for(i in 1:length(row_vector)){ #same as in part 1
  
  row <- input[row_vector[i]] #iterate through each row 
  position <- position_vector[i] #Count trees at this position
  
  if(position > stringr::str_count(row)){ #needed if pattern exceeds to the right
    repeat{
      if(!exists("x")){x <<- 1}
      
      row <- stringr::str_c(rep(row, x +1), collapse = "") #Paste together as many patterns as needed 
      x <<- x + 1
      
      if(position <= stringr::str_count(row)){
        break #stop if pattern long enough
      }
      
    } 
  }
  
  trees <- stringr::str_locate_all(row, "\\#") %>% #find location of trees
    unlist() %>% #str_locate_all makes a list, therefore unlist and throw out duplicates
    unique()
  
  if(position %in% trees){ 
    if(!exists("count")){
      count <- 1 #initialize count 
      #print(stringr::str_c("Tree here at position ", position))
    } else {
      count <- count + 1 #count every time a tree occurs
      #print(stringr::str_c("Tree here at position ", position))
    }
  } 
  
}
  no_trees <- count
  rm(count)
  return(no_trees)
}


result <- purrr::map(input_tibbles, slopes, input = input) %>% #Iterate through each slope's row/position tibble
  unlist() %>% 
  prod()
