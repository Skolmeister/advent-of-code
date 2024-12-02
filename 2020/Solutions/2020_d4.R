library(magrittr)


# Parse Input
input <- readLines("~/R Projects/adventofcode/2020/Input/2020_d4_input.txt") %>% 
  stringr::str_c(collapse = "|") %>% 
  stringr::str_split("\\|\\|") %>% # Split rows, as double-| signals row change
  unlist() %>% 
  stringr::str_replace_all("\\|", " ")


# Part 1 -----------------------------------------------------------------

required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
optional_field <- c("cid")

check_passport <- function(row_no, input){

  fields <- stringr::str_extract_all(input[row_no], "([a-z]{3})(?=:)") %>% #Extract all fields with regex
    unlist() 
  
  if(length(fields) < 7 | !all(required_fields %in% fields)){ #If less than 7 fields its invalid , if not all required fields, its invalid, too
  return(tibble::tibble(row_number = row_no,
                        check = FALSE))
  } else {tibble::tibble(row_number = row_no,
                         check = TRUE)}
  }


result <- purrr::map_df(c(1:length(input)), check_passport, input = input) %>% #map over input vector
  dplyr::count(check) #TRUE Values are the result


# Part 2 ------------------------------------------------------------------

format_passport_info <- function(row_no){
stringr::str_split(input[row_no], "\\s") %>% 
  unlist() %>% 
  dplyr::as_tibble() %>%
  tidyr::separate(value, into = c("field", "value"), sep = ":", convert = TRUE)%>%
  dplyr::filter(field != "cid") %>% 
  tidyr::pivot_wider(names_from = "field",
                     values_from = "value")
  }

formatted_passports <- purrr::map_df(c(1:length(input)), format_passport_info) %>% 
  tibble::rowid_to_column()
 
  formatted_passports %>% 
  dplyr::mutate_at(.vars = dplyr::vars(2,5,6), .funs=as.integer) %>% 
  tidyr::separate(hgt, into = c("hgt", "type"), sep = -2, convert = TRUE) %>% 
  dplyr::filter(dplyr::between(byr, 1920, 2002),
                dplyr::between(iyr, 2010, 2020),
                dplyr::between(eyr, 2020, 2030),
                stringr::str_detect(hcl, "^#([a-f0-9]{6})"),
                ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
                stringr::str_count(stringr::str_extract(pid, "\\d*")) == 9,
                ifelse(type == "cm", dplyr::between(hgt, 150, 193), dplyr::between(hgt, 59,76))) %>% 
  nrow()
