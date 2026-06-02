#' Load plastic pollution data
#'
#' This function loads the original plastic pollution dataset from TidyTuesday.
#'
#' @return A data frame containing the plastic pollution data.
#' @importFrom readr read_csv
#' @export

load_data <- function(){
  path <- system.file("merged_data.csv", package = "auditr")
  arrow::read_csv_arrow(path, as_data_frame = TRUE)
}


