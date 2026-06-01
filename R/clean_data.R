#' Clean plastic pollution data
#'
#' Loads the plastic pollution dataset, replaces missing plastic-type counts
#' with zero, and creates a row-level total.
#'
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate across
#' @importFrom stringr str_to_title
#'
#' @return A cleaned tibble used internally by package functions.
#'




clean_data <- function(){
  dat <- load_data()

  dat_clean <- dat |>
    mutate(across(empty:pvc, ~tidyr::replace_na(., 0)),
           total = rowSums(across(c(empty:pvc))),
           .before = num_events,
           country = str_to_title(country))
}
