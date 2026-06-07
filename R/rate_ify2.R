#' rate_ify2
#'
#' turns selected columns into rates based on a denominator column
#'
#' @param df a dataset with information to be rate-ified
#' @param numerators single column or vector of columns to be rate-ified. Tidy eval compatible
#' @param denominator single denominator column used to create the rate along with the numerators
#'
#' @importFrom dplyr mutate across
#'
#'
#' @returns a data set with rate-ified columns
#' @export
#'
#' @examples
#' rate_ify2(df = load_data(), numerator = hdpe:pvc, denominator = grand_total)
rate_ify2 <- function(df = load_data(),
                      numerators = empty:pvc,
                      denominator = grand_total) {
  stopifnot(is.data.frame(df))

  df |>
    dplyr::mutate(dplyr::across(.cols = {{ numerators }},
                  .fns = ~ ifelse({{denominator}} == 0,
                                  NA_real_,
                                  .x / {{ denominator }}),
                  .names = "{.col}"))
}
