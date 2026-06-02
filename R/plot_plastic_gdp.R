#' density_plotter
#'
#' Plots a density ridge plot comparing the density of plastic type audit amounts to gdp based on user inputs
#'
#' @param data a data frame with plastic audit numbers and gdp figures
#' @param selected_year optional, default 2019. 2019 or 2020 for our data
#' @param cutoff optional, default 3500. the cutoff for the density plot x axis. Can be useful to ignore large out liars or to zoom into the most common audit amounts
#' @param chosen_types optional, default includes: unknown, hdpe, ldpe, other, pet, pp, ps, pvc. what type of plastic is to be compared to eachother
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 aes labs theme

#'
#' @returns a plot
#' @export
#'
#' @examples density_ploter(data,
#'                          selected_year = 2020,
#'                          cutoff = 2000,
#'                          chosen_types = c("hdpe", "pvc", "other"))
density_ploter <- function(data, selected_year = 2019,
                           cutoff = 3500,
                           chosen_types = c("unknown", "hdpe", "ldpe","other",
                                            "pet", "pp", "ps", "pvc")) {

  allowed_types <- c("unknown", "hdpe", "ldpe","other",
                     "pet", "pp", "ps", "pvc")

  if (!is.character(chosen_types)) {
    stop("chosen_types must be a character vector.", call. = FALSE)
  }

  if (!all(chosen_types %in% allowed_types)) {
    stop("plastic_type is not in dataframe")
  }

  allowed_years <- c(2019, 2020)
  if (!selected_year %in% allowed_years) {
    stop("year must be either 2019 or 2020")
  }

  if (!is.numeric(selected_year) || length(selected_year) != 1) {
    stop("selected_year must be one numeric value.", call. = FALSE)
  }

  if (!is.numeric(cutoff) || length(cutoff) != 1 || cutoff <= 0) {
    stop("cutoff must be one positive numeric value.", call. = FALSE)
  }



  data |>
    filter(year == selected_year) |>
    dplyr::group_by(country) |>
    dplyr::summarize(unknown = sum(empty),
                     hdpe = sum(hdpe),
                     ldpe = sum(ldpe),
                     other = sum(o),
                     pet = sum(pet),
                     pp = sum(pp),
                     ps = sum(ps),
                     pvc = sum(pvc),
                     total = sum(total),
                     gdp_per_capita = mean(gdp_per_capita),
                     hdi = mean(HDI),
                     .groups = "drop") |>
    mutate(income_group = dplyr::case_when(
      gdp_per_capita < 1500 ~ "Low Income",
      gdp_per_capita > 12000 ~ "High Income",
      TRUE ~ "Middle Income")) |>
    tidyr::pivot_longer(
      cols = unknown:pvc,
      names_to = "plastic_type",
      values_to = "waste") |>
    filter(waste <= cutoff, plastic_type %in% chosen_types) |>

    ggplot2::ggplot(mapping = aes(x = waste,
                                  y = plastic_type,
                                  fill = income_group)) +
    ggridges::geom_density_ridges(alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c(
      "High Income"   = "#E69F00",
      "Middle Income" = "#009E73",
      "Low Income"    = "#0072B2")) +
    labs(
      title = glue::glue("Plastic Waste Type Distribution of Countries by GDP per Capita ({selected_year})"),
      subtitle = "Divided into <span style='color: #E69F00;'>High Income</span>,
      <span style='color: #009E73;'>Middle Income</span>,
    and <span style='color: #0072B2;'>Low Income</span>",
      x = glue::glue("Plastic Waste Units Audited cut at {cutoff}"),
      y = "",
      alt = "Density ridge plot showing the density of plastic waste audited
    for multiple types of plastic. Colored by GDP per capita category,
    High, Middle, and Low") +
    ggplot2::theme_minimal() +
    theme(plot.subtitle =  ggtext::element_markdown(),
          legend.position = "none")
}
