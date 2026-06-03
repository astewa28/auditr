#' stat_analysis
#'
#' Fits a mixed-effects model analyzing log total plastic pollution based on
#' user-selected predictors with Country as a random effect and creates a
#' formatted Type III ANOVA table for the mixed-effects model
#'
#' @param fixed_vars Character vector of fixed-effect predictor variables to
#' include in the mixed-effects model. Defaults to log population,
#' log population density, log GDP, year, and region.
#'
#'
#' @returns A formatted gt table containing the specified variables in the model
#'
#' @importFrom dplyr group_by summarise mutate recode
#' @importFrom tibble rownames_to_column
#' @importFrom lmerTest lmer
#' @importFrom stats anova as.formula
#' @importFrom gt gt tab_header md fmt_number tab_style cell_text cells_body
#' @importFrom tidyselect where
#'
#' @export
#'
#' @examples stat_analysis(chosen_types = c("log_population", "year", "region"))

stat_analysis <- function(fixed_vars = c("Population",
                                         "pop_density",
                                         "gdp",
                                         "year",
                                         "region")){


  if (!is.character(fixed_vars)) {
    stop("fixed_vars must be a character vector.", call. = FALSE)
  }

  if (length(fixed_vars) == 0) {
    stop("fixed_vars must contain at least one predictor variable.", call. = FALSE)
  }

  if (anyNA(fixed_vars)) {
    stop("fixed_vars cannot contain NA values.", call. = FALSE)
  }

  if (any(fixed_vars == "")) {
    stop("fixed_vars cannot contain empty strings.", call. = FALSE)
  }

  first_nonmissing <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else x[1]
  }


  model_var_map <- c(
    Population = "log_population",
    pop_density = "log_pop_density",
    gdp = "log_gdp_billions",
    year = "year",
    region = "region"
  )

  valid_fixed_vars <- names(model_var_map)

  invalid_vars <- setdiff(fixed_vars, valid_fixed_vars)

  if (length(invalid_vars) > 0) {
    stop(
      paste0(
        "Invalid fixed_vars: ",
        paste(invalid_vars, collapse = ", "),
        ". Valid choices are: ",
        paste(valid_fixed_vars, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  model_df <- load_data() |>
    dplyr::group_by(country, year) |>
    dplyr::summarise(
      total_plastic = sum(total, na.rm = TRUE),
      Population_millions = first_nonmissing(Population) / 1000000,
      pop_density = first_nonmissing(pop_density),
      gdp_billions = first_nonmissing(gdp) / 1000000000,
      region = first_nonmissing(region),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      country = factor(country),
      year = factor(year),
      region = factor(region),
      log_total_plastic = log(total_plastic),
      log_population = log(Population_millions),
      log_pop_density = log(pop_density),
      log_gdp_billions = log(gdp_billions)
    )

  dep_var <- "log_total_plastic"

  random_var <- "country"

  model_fixed_vars <- unname(model_var_map[fixed_vars])

  formula_string <- sprintf("%s ~ %s + (1 | %s)",
                            dep_var,
                            paste(model_fixed_vars, collapse = " + "),
                            random_var)



  model_formula <- as.formula(formula_string)
  fit_final_maybe <- lmer(formula = model_formula, data = model_df)


  label_map <- c(
    log_total_plastic = "Log Total Plastic",
    log_population = "Log Population",
    log_pop_density = "Log Population Density",
    log_gdp_billions = "Log GDP (billions)",
    total_plastic = "Total Plastic",
    Population_millions = "Population (millions)",
    pop_density = "Population Density",
    gdp_billions = "GDP (billions)",
    year = "Year",
    region = "Region",
    country = "Country"
  )


  anova(fit_final_maybe) |>
    as.data.frame() |>
    tibble::rownames_to_column("Predictor") |>
    dplyr::mutate(
      Predictor = dplyr::recode(
        Predictor,
        !!!label_map,
        .default = Predictor
      )
    ) |>
    gt() |>
    tab_header(
      title = md("**Type III ANOVA for Final Mixed Model**"),
      subtitle = "Satterthwaite's method"
    ) |>
    fmt_number(
      columns = where(is.numeric),
      decimals = 3
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = Predictor == "Log Population"
      )
    )
  }
