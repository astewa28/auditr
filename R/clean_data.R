#' Cleans data loaded from load_dat
#'
#' @importFrom tidyr replace_na
#'
#' @importFrom dplyr mutate across
#'
#' @return A cleaned dataset to be used for data analysis
#'


clean_data <- function(){
  dat <- load_data()

  dat_clean <- dat |>
    mutate(across(empty:pvc, ~tidyr::replace_na(., 0)),
           total = rowSums(across(c(empty:pvc))),
           .before = num_events)
}
