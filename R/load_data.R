#' Load merged plastic pollution data
#'
#' This function loads the merged plastic pollution dataset stored in the package.
#'
#' @return A data frame containing the merged plastic pollution data.
#' @importFrom arrow read_csv_arrow
#' @export

load_data <- function(){
  path <- system.file("merged_data.csv", package = "auditr")
  arrow::read_csv_arrow(path, as_data_frame = TRUE)
}


