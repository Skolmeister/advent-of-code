# Advent of Code Day 2
# Part 1

intcode <- function(x){
  
  index <- 0 # Needed for skipping to next opcode
  opcode <- x[index + 1] # R indexes first vector value as 1 instead of 0 

while(opcode != 99){ # opcode 99 halts the program

if(opcode == 1){ 
  
  x[x[index+4]+1] <- x[x[index+2]+1] + x[x[index + 3]+1] #subset the value which has to change, operation for opcode 1
  
  } else if (opcode == 2){
    
    x[x[index+4]+1] <- x[x[index+2]+1] * x[x[index + 3]+1] #subset the value which has to change, operation for opcode 1             
   } 
  
  index <- index + 4 # skip 4
  
  opcode <- x[index + 1] # get new opcode
  
}
  
  print(x)
}

input <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,2,13,23,27,2,27,13,31,2,31,10,35,1,6,35,39,1,5,39,43,1,10,43,47,1,5,47,51,1,13,51,55,2,55,9,59,1,6,59,63,1,13,63,67,1,6,67,71,1,71,10,75,2,13,75,79,1,5,79,83,2,83,6,87,1,6,87,91,1,91,13,95,1,95,13,99,2,99,13,103,1,103,5,107,2,107,10,111,1,5,111,115,1,2,115,119,1,119,6,0,99,2,0,14,0)
input[2] <- 12
input[3] <- 2

answer <- intcode(input)


# Part 2

output <- 19690720 #Desired output

for (i in 1:99) {  # Double for-Loop as described by https://github.com/Morawski21 | Still trying to figure out how to do it with purrr
  for (j in 1:99) {
    test[2] <- i
    test[3] <- j
    
    result <- intcode(test)[1]
    
    if (result == output) {
      noun <- i
      verb <- j
    }
  }
}

answer <- 100 * noun + verb # Getting the answer





