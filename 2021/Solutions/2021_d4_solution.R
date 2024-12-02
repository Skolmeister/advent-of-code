# Day 4
library(magrittr)

# Cleaning the input
input_path <- "~/R Projects/adventofcode/2021/Input/2021_d4_input"

input <- readLines(input_path)

draw <- as.integer(unlist(strsplit(input[1],",")))

boards <- input[2:length(input)]
boards <- stringr::str_trim(boards[boards != ""])

cleaned_boards <- boards[!is.na(boards)]

all_boards <- list() #Build matrices from the input for each board

for (i in (1:(length(cleaned_boards) / 5))){
  if(i == 1){
    all_boards[[i]] <- matrix(
      as.integer(
        unlist(
          strsplit(cleaned_boards[(i):(5*i)], "\\s{1,3}")
        )
      ), nrow = 5, ncol = 5, byrow = TRUE)  
  }
  
  all_boards[[i]] <- matrix(
    as.integer(
      unlist(
        strsplit(cleaned_boards[(((i-1)*5)+1):(i*5)], "\\s{1,3}")
      )
    ), nrow = 5, ncol = 5, byrow = TRUE)
}

# Part 1 ------------------------------------------------------------------

check_board <- function(board, draw){
  
  #Initialize vector of marked numbers - At the beginning none are marked
  marked <- c()
  
  for(draw_no in draw){
    current_board <- all_boards[[board]]
    new_marked <- current_board[current_board==draw_no] # Add the new drawn number to the marked vector
    if(length(new_marked) != 0){
      marked <- c(marked, new_marked)
    }
  
    
    line_index <- which(current_board == draw_no, arr.ind = T) # Check row and column index
    lines <- list("row" = current_board[line_index[1],],
               "col" = current_board[,line_index[2]])
    
    if(all(lines[["row"]] %in% marked)){ #If all values in row are already marked, this row is finished
      
      draws_needed <- which(draw == draw_no) #How many draws were needed?
      
      cat("Board", board, "Row Number", line_index[1], "finished within",draws_needed,"turns!\n\n")
      
      unmarked <- c(current_board)[!c(current_board) %in% marked] #Get the unmarked numbers on  the board
      result <- sum(unmarked) * draw_no #Calculate result
      
      return(
        tibble::tibble(
        "board" = board,
        "draws_needed" = draws_needed,
        "result" = result)
        )
      
    }
    
    if(all(lines[["col"]] %in% marked)){
      
      draws_needed <- which(draw == draw_no)
      
      cat("Board", board, "Col Number", line_index[2], "finished within",draws_needed,"turns!\n\n")
      
      unmarked <- c(current_board)[!c(current_board) %in% marked]
      result <- sum(unmarked) * draw_no
      
      return(tibble::tibble(
        "board" = board,
        "draws_needed" = draws_needed,
        "result" = result)
      )
      
    }
    
  }
  
}

#Run this function for every board and get the board with minimum draws needed
purrr::map_df(1:length(all_boards), check_board, draw = draw) %>% 
  dplyr::filter(draws_needed == min(draws_needed))


# Part 2 ------------------------------------------------------------------

#This is now very easy as you just have to change minimum to maximum - MUCH SUCCESS!
purrr::map_df(1:length(all_boards), check_board, draw = draw) %>% 
  dplyr::filter(draws_needed == max(draws_needed))

