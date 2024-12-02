# Day 2

# Read Input
input <- readr::read_delim(
    here::here("2022", "input", "2022_d2_input.txt"), 
    delim = " ", 
    col_names = FALSE) |> 
    dplyr::rename("opp" = X1, 
                  "strategy" = X2)

# Part 1
# Scoring Table
scores <- tibble::tibble(opp = c("A", "B", "C"),
                         strategy = c("X", "Y", "Z"), 
                         points = c(1,2,3))

part_1 <- input |> 
    dplyr::left_join(scores[,c("opp", "points")], 
        by = "opp") |> 
    dplyr::left_join(scores[,c("strategy", "points")], 
        by = "strategy", 
        suffix = c("", "_strat")) |> 
    dplyr::mutate( # Calculate Match results
        result = dplyr::case_when(
        points_strat - points == 1 ~ 6,
        points_strat - points == 2 ~ 0,
        points_strat - points == 0 ~ 3,
        points_strat - points == -2 ~ 6,
        TRUE ~ 0
        ),
        score = points_strat + result 
    )

sum(part_1$score)
    
# Part 2

part_2 <- input |> 
    dplyr::left_join(scores[,c("opp", "points")], 
        by = "opp") |> 
    dplyr::rename("points_opp" = points) |> 
    dplyr::mutate( # Get the result needed
        score_needed = dplyr::case_when(
            strategy == "X" ~ 0,
            strategy == "Y" ~ 3,
            strategy == "Z" ~ 6,
        ),
        points_own = dplyr::case_when( # Calculate what you should show
            score_needed == 6 & points_opp < 3 ~ points_opp + 1,
            score_needed == 6 & points_opp == 3 ~ 1,
            score_needed == 3 ~ points_opp,
            score_needed == 0 &  points_opp > 1 ~ points_opp - 1,
            score_needed == 0 &  points_opp == 1 ~ 3,

        ), 
        score = score_needed + points_own    
    )

sum(part_2$score)
