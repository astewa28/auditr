#' Load plastic pollution data
#'
#' This function loads the original plastic pollution dataset from TidyTuesday.
#'
#' @return A data frame containing the plastic pollution data.
#' @importFrom readr read_csv
#' @export

load_data <- function(){

  arrow::read_csv_arrow('inst/merged_data.csv')

}


