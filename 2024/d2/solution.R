input <- readLines(here::here("2024", "d2", "input")) |>
  strsplit("\\s")

check_report <- function(report) {
  report_to_check <- as.integer(report)
  increasing <- sort(report_to_check)
  decreasing <- sort(report_to_check, decreasing = TRUE)

  if (identical(report_to_check, increasing) | identical(report_to_check, decreasing)) {
    diff_vector <- abs(diff(report_to_check))
    if (all(diff_vector >= 1 & diff_vector <= 3)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

part1 <- purrr::map_lgl(input, check_report) |>
  sum()

problem_dampener <- function(report) {
  iterations <- length(report)

  if (check_report(report) == FALSE) {
    reports_to_check <- purrr::map(
      seq_len(iterations),
      \(x) report[-x]
    )

    correct_reports <- purrr::map_lgl(reports_to_check, check_report)
    if (all(correct_reports == FALSE)) {
      return(FALSE)
    }

    if (length(correct_reports[correct_reports == TRUE]) > 1) {
      dupe <- reports_to_check[which(correct_reports == TRUE)]
      report <- unlist(dupe[1])
    } else {
      report <- unlist(reports_to_check[which(correct_reports == TRUE)])
    }
  }

  return(check_report(report))
}

part2 <- purrr::map_lgl(input, problem_dampener) |>
  sum()


