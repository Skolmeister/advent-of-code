library(magrittr)

# Cleaning the input ------------------------------------------------------
input_path <- "~/R Projects/adventofcode/2021/Input/2021_d5_input"

input <- readLines(input_path)

split <- lapply(strsplit(input, " -> "), strsplit, ",")
 
getlines <- function(element){
  element <- lapply(element, as.integer)
  df <- tibble::tibble(x1 = element[[1]][[1]],
                       y1 = element[[1]][[2]],
                       x2 = element[[2]][[1]],
                       y2 = element[[2]][[2]],
                       direction = dplyr::case_when(
                         x1 == x2 ~ "vertical", 
                         y1 == y2 ~ "horizontal",
                         abs(x2 - x1) == abs(y2-y1) ~ "diagonal", #Part 2
                         TRUE ~ "No Direction"
                         )
                       )
}

lines <- purrr::map(split, getlines) 


# Function for the Grid ---------------------------------------------------
points <- function(line){
  
  if (line$direction == "horizontal") {
    startx <- line$x1
    endx <- line$x2
    
    points <- tibble::tibble(x = seq(startx, endx),
                             y = line$y1)
    return(points)
    
  } else if(line$direction == "vertical"){
    starty <- line$y1
    endy <- line$y2
    
    points <- tibble::tibble(y = seq(starty, endy),
                             x = line$x1)
    return(points)
  } else if(line$direction == "diagonal"){ #Part 2
    points <- tibble::tibble(x = seq(line$x1, line$x2),
                             y = seq(line$y1, line$y2))
    return(points)
  }
}

#Run function for every coord pair
purrr::map_df(lines, points) %>% 
  dplyr::count(x, y, sort = T) %>% 
  dplyr::filter(n >= 2) %>% 
  nrow()


