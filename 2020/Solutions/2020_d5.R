library(magrittr)

input <- readLines("~/R Projects/adventofcode/2020/Input/2020_d5_input.txt") 


# Part 1 ------------------------------------------------------------------


parse_boardingpass <- function(input){
rows <- c(1:127)
seats <- c(0:7)

seat_row <- stringr::str_extract(input, "^[F|B]{7}") %>% 
  stringr::str_split("") %>% 
  unlist()
seat_position <- stringr::str_extract(input, "[L|R]{3}$") %>% 
  stringr::str_split("") %>% 
  unlist()

for(i in c(1:7)){
  if(seat_row[i]=="F"){
    row_min <- min(head(rows, length(rows)/2))
    row_max <- max(head(rows, length(rows)/2))
    rows <- c(row_min:row_max)
  } else {
    row_min <- min(tail(rows, length(rows)/2))
    row_max <- max(tail(rows, length(rows)/2))
    rows <- c(row_min:row_max)
  }
}

for(i in c(1:3)){
  
  if(seat_position[i]=="L"){
    seat_left <- min(head(seats, length(seats)/2))
    seat_right <- max(head(seats, length(seats)/2))
    seats <- c(seat_left:seat_right)
  } else {
    seat_left <- min(tail(seats, length(seats)/2))
    seat_right <- max(tail(seats, length(seats)/2))
    seats <- c(seat_left:seat_right)
  }
  
}

seat_position <- tibble::tibble(row = rows,
                                seat = seats,
                                id = row * 8 + seat)

return(seat_position)
}


seat_ids <- purrr::map_df(input, parse_boardingpass)

result <- seat_ids %>% 
  dplyr::filter(id == max(id))


# Part 2 ------------------------------------------------------------------

all_possible_ids <- c((1*8)+0:(127*8)+7) 
possible_seats <- all_possible_ids[!all_possible_ids %in% seat_ids$id]

for(i in possible_seats){
  if((possible_seats[i]+ 1) %in% seat_ids$id & (possible_seats[i]- 1) %in% seat_ids$id){
    result2 <- possible_seats[i]
  }
}

