input_raw <- strsplit(readLines(here::here("2023", "data", "day12.txt")), " ")

i <- 2

records <- input_raw[[2]][1]
record_groups <- as.integer(unlist(strsplit(input_raw[[2]][2], ",")))


length_record <- nchar(records)
count_broken <- sum(record_groups)


all_groups <- unlist(stringr::str_extract_all(records, "(#|\\?)+"))

divide_in_parts <- length(record_groups)

for (group in seq_along(all_groups)) {
  broken_should <- record_groups[group]
  expanded_group <- paste0(".", all_groups[group], ".")

  
}