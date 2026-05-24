#' Load plastic pollution data
#'
#' This function loads the original plastic pollution dataset from TidyTuesday.
#'
#' @return A data frame containing the plastic pollution data.
#' @importFrom readr read_csv
#' @export

load_data <- function(){

  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv')

}
