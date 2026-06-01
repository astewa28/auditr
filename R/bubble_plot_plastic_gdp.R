#' Plot Plastic Pollution vs. GDP as a Bubble Chart
#'
#' Creates bubble charts showing the relationship between a country's
#' GDP and the amount of plastic pollution found per cleanup event, for the
#' years 2019 and 2020.
#'
#' @param countries A character vector of country names to include in the plot.
#'
#' @return A bubble plot showing total plastic pollution vs GDP where bubble
#'  size is population.
#'
#' @importFrom dplyr filter mutate group_by summarize
#' @importFrom ggplot2 ggplot aes geom_point scale_size labs coord_cartesian
#'   scale_x_sqrt facet_wrap theme_light scale_color_manual
#' @importFrom countrycode countrycode
#'
#' @examples
#' bubble_plotter(c("United States of America", "Canada", "Nigeria"))
#' bubble_plotter("Taiwan")
#'
#' @export



bubble_plotter <- function(countries) {
  dat <- load_data()

  dat |>
    filter(country %in% countries) |>
    mutate(
      region = countrycode::countrycode(country,
                                        origin = "country.name",
                                        destination = "continent"),
      total_normalized = total / num_events,
      gdp_trillions = gdp / 1e6
    ) |>
    filter(
      total != 0,
      !is.na(total),
      !is.na(gdp_trillions),
      gdp_trillions != 0,
      !is.na(region)
    ) |>
    group_by(country, region, year) |>
    summarize(
      total_plastic = sum(total_normalized, na.rm = TRUE),
      gdp_trillions = mean(gdp_trillions, na.rm = TRUE),
      Population = mean(Population, na.rm = TRUE)
    ) |>
    ggplot(aes(x = gdp_trillions, y = total_plastic, size = Population,
               color = region)) +
    geom_point(alpha = 0.5) +
    scale_size(range = c(.1, 15), name = "Population") +
    labs(
      color = "Region",
      x = "GDP (Trillions)",
      title = "Plastic Pollution Found in Audits vs. GDP for 2019 and 2020",
      subtitle = "Total Plastic per Cleanup Event",
      y = ""
    ) +
    coord_cartesian(ylim = c(0, 26000), xlim = c(0, 25)) +
    scale_x_sqrt() +
    facet_wrap(~year) +
    theme_light() +
    scale_color_manual(values = c(
      "Africa" = "#E69F00",
      "Americas" = "#56B4E9",
      "Asia" = "#009E73",
      "Europe" = "#F0E442",
      "Oceania" = "#0072B2"
    ))
}
