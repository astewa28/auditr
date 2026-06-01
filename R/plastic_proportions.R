#' Table of the proportions of each plastic type
#'
#' This function creates a table displaying the proportion of each plastic type for a specified country and year.
#'
#' @param countries A character vector of one or more countries to be included in the table
#'
#' @param years A numeric or character vector of one or two years to be included in the table
#'
#' @importFrom dplyr mutate group_by summarize filter across
#'
#' @importFrom gt gt fmt_percent tab_style tab_caption cols_move_to_end cols_label cell_text cells_column_labels
#'
#' @return A GT table with proportions.
#'
#' @export
#'
#' @examples
#' plastic_proportions(c("Canada", "Argentina"), 2019)
#' plastic_proportions("China", 2020)



plastic_proportions <- function(countries = NULL, years = NULL){

  if (!is.null(countries) && !is.character(countries)) {
    stop("countries must be a character vector.", call. = FALSE)
  }

  if (!is.null(years) && !(is.numeric(years) || is.character(years))) {
    stop("years must be a numeric or character vector.", call. = FALSE)
  }

  if (!is.null(countries) && length(countries) == 0) {
    stop("countries must contain at least one country.", call. = FALSE)
  }

  if (!is.null(years) && length(years) == 0) {
    stop("years must contain at least one year.", call. = FALSE)
  }
  dat <- clean_data()

  result <- dat |>
    dplyr::mutate(year = factor(year)) |>
    dplyr::group_by(country, year) |>
    dplyr::summarize(
      dplyr::across(empty:grand_total, ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(empty:pvc, ~ .x / grand_total)) |>
    dplyr::select(-grand_total)

  if (!is.null(countries)) {
    result <- dplyr::filter(result, country %in% countries)
  }

  if (!is.null(years)) {
    result <- dplyr::filter(result, year %in% as.character(years))
  }

  result |>
    gt::gt() |>
    gt::fmt_percent(columns = empty:pvc) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_caption("Percentage of Plastic Type Found in Audits From the Top 5 Populated
              Countries") |>
    gt::cols_move_to_end(columns = o) |>
    gt::cols_label(
      country = "Country",
      year    = "Year",
      empty   = "Empty",
      hdpe    = "HDPE",
      ldpe    = "LDPE",
      o       = "Other",
      pet     = "PET",
      pp      = "PP",
      ps      = "PS",
      pvc     = "PVC"
    )
}

