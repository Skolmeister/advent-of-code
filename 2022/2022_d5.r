
get_input <- function() {
    # Parsing the input was the really difficult part
    input_path <- here::here("2022", "input", "2022_d5_input.txt")
    input_unparsed <- readLines(input_path)
    input_procedure <- input_unparsed[stringr::str_detect(input_unparsed, "move")]

    stacks <- vroom::vroom_fwf(I(input_unparsed[1:which(input_unparsed == "") - 1]))
    names(stacks) <- stacks[nrow(stacks), ]
    stacks <- stacks[1:nrow(stacks) - 1, ] |>
        dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) stringr::str_remove_all(x, "\\[|\\]")))

    stacks_list <<- purrr::map(as.list(stacks), \(x) x[!is.na(x)])

    procedure <<- readr::read_delim(I(input_procedure),
        delim = " ", col_names = FALSE
    ) |>
        dplyr::select(
            amount = "X2",
            from = "X4",
            to = "X6"
        ) |>
        dplyr::mutate(step = dplyr::row_number())

    return(list(
        stacks = stacks_list,
        procedure = procedure
    ))
}


stacks_list <- get_input()$stacks

part <- 2

for (i in procedure$step) {
    instruction <- procedure |>
        dplyr::filter(step == i)

    stack_new <- stacks_list

    crate_move <- stacks_list[[instruction$from]][1:instruction$amount]
    if (part == 1) { # Part 1 takes crates top -> down
        crate_move <- rev(crate_move)
    } # Part 2 Grabs them together
    stack_new[[instruction$to]] <- c(crate_move, stacks_list[[instruction$to]]) # On top of the stack
    stack_new[[instruction$from]] <- stacks_list[[instruction$from]][-seq_along(1:instruction$amount)]

    stacks_list <<- stack_new

    if (any(unlist(lapply(stacks_list, is.na)))) {
        print(instruction_no)
    }
}

stringr::str_c(unlist(purrr::map(stacks_list, \(x) x[1])), collapse = "")
