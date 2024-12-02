#Day 7 The Treachery of Whales
# Input -------------------------------------------------------------------

input_path <- "~/R Projects/adventofcode/2021/Input/2021_d7_input"
input <- scan(input_path,
              sep = ",")
# Part 1 ------------------------------------------------------------------

min(sapply(min(input):max(input), function(x){sum(abs(input-x))})) 
# Fuel is the distance to the desired spot, sum everything up, find the minimum

# Part 2 ------------------------------------------------------------------

#Now fuel is the sum of the vector 1:steps for each crab submarine, just add anonter sapply 
min(sapply(min(input):max(input), function(x){sum(sapply(abs(input - x), function(x){ sum(1:x)}))}))
