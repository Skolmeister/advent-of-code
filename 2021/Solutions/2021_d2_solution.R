library(magrittr)

#Day 2: Dive!

input_path <- "~/R Projects/adventofcode/2021/Input/2021_d2_input"

input <- readLines(input_path)

testinput <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

#Tidy the input
split_commands <- input %>% 
  dplyr::as_tibble() %>% 
  tidyr::separate(value, 
                  into = c("command", "units"), 
                  sep = "\\s", #Split by whitespace
                  convert = TRUE) #Converts unit to integer


# Part 1 -----------------------------------------------------------------

#Submarine Commands:
# Forward X -> Increases horizontal by X
# Up / Down X -> In / Decreases vertical

get_position <- function(command_no, data){
  if(command_no == 1){ #Initialize start depth
    position_vector <<- c(0,0) #First element is horizontal (x), second is depth (-y)
  }
  
  #Commands from Puzzle
  if(data$command[command_no] == "forward"){
    position_vector[1] <<- position_vector[1] + data$units[command_no]
  } else if(data$command[command_no] == "down"){
    position_vector[2] <<- position_vector[2] + data$units[command_no]
  } else if(data$command[command_no] == "up"){
    position_vector[2] <<- position_vector[2] - data$units[command_no]
  }
  
  #Just for nice output, could just run prod(position_vector) in the end
  return(tibble::tibble(command = data$command[command_no],
                        x = position_vector[1],
                        y = position_vector[2],
                        puzzle_solution = x * y))
}

result <- purrr::map_df(1:length(input), get_position, data = split_commands) %>% 
  dplyr::arrange(desc())



# Part Two ----------------------------------------------------------------

#Down / Up increases new variable AIM
#Forward increases horizontal and depth by mutliplying depth with aim

get_position_aim <- function(command_no, data){
  if(command_no == 1){ #Initialize start depth
    position_vector <<- c(0,0,0) #First element is horizontal (x), second is depth (y), third is aim
  }
  
  #Commands from Puzzle
  if(data$command[command_no] == "forward"){
    position_vector[1] <<- position_vector[1] + data$units[command_no]
    position_vector[2] <<- position_vector[2] + position_vector[3] * data$units[command_no]
  } else if(data$command[command_no] == "down"){
    position_vector[3] <<- position_vector[3] + data$units[command_no]
  } else if(data$command[command_no] == "up"){
    position_vector[3] <<- position_vector[3] - data$units[command_no]
  }
  
  #Just for nice output, could just run prod(position_vector) in the end
  return(tibble::tibble(command_no = command_no,
                        command = data$command[command_no],
                        x = position_vector[1],
                        y = position_vector[2],
                        aim = position_vector[3],
                        puzzle_solution = x * y))
}

result <- purrr::map_df(1:length(input), get_position_aim, data = split_commands) %>% 
  dplyr::arrange(desc(command_no))

# For the GIF 
ggplot2::ggplot(result)+
  ggplot2::geom_point(ggplot2::aes(x, -y), colour = "#00008b", alpha = 0.7)+
  ggimage::geom_image(ggplot2::aes(x = x, y = -y, image = "~/R Projects/adventofcode/2021/Input/submarine.png"), size = 0.1, asp = 1.618)+
  ggplot2::scale_x_continuous(expand = c(0,0))+
  ggplot2::scale_y_continuous(expand = c(0,0))+
  ggplot2::theme_void()+
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#00008b"),
                 aspect.ratio = 1 / 1.618) +
  gganimate::transition_time(x)

gganimate::anim_save("~/R Projects/adventofcode/2021/day2.gif")
