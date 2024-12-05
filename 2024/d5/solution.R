input <- readLines(here::here("2024", "d5", "input"))

rules <- input[1:(which(input == "")-1)] |>
  stringr::str_extract_all("\\d+") |>
  purrr::map(as.integer)

updates <- input[(which(input == "") + 1):length(input)] |>
  stringr::str_extract_all("\\d+") |>
  purrr::map(as.integer)


rules_for_update <- function(update) {
  rl <- purrr::map(
    rules, 
    \(x) check_rule(update, x)
  )

  return(rl[which(lengths(rl) > 0)])
}

check_rule <- function(update, rule) {
  if (all(rule %in% update)) {
    return(rule)
  }
}

apply_rule <- function(update, rule) {
  indices <- purrr::map_dbl(
    rule,
    \(x) get_position(update, x)
  )
  
  if (identical(indices, sort(indices))) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}

get_position <- function(update, rulepart) {
  which(update == rulepart)
}

check_update <- function(update, part) {
  relevant_rules <- rules_for_update(update)
  ruleresults <- purrr::map_lgl(
    relevant_rules,
    \(x) apply_rule(update, x)
  )

  if (all(ruleresults == TRUE)) {
    middle_value <- ceiling(length(update) / 2)
    return(update[middle_value])
  } else {
    if (part == 1) {
      return(0)
    } else {
      rule_broken <- unlist(relevant_rules[which(ruleresults == FALSE)])
      index_first_item <- which(update == rule_broken[2])
      index_second_item <- which(update == rule_broken[1])
      update[index_first_item] <- rule_broken[1]
      update[index_second_item] <- rule_broken[2]
      check_update(update, part = 2)
    }
  }
}

result_rule_check <- purrr::map_int(
  updates,
  \(x) check_update(x, part = 1)
) 

part1 <- result_rule_check |>
sum()

updates_to_sort <- updates[which(result_rule_check == 0)]

new_sorted <- purrr::map_int(
  updates_to_sort,
  \(x) check_update(x, part = 2)
) 

part2 <- new_sorted |>
sum()
