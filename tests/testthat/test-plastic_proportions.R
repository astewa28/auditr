

testthat::test_that("summarize plastic proportions works", {

  testthat::expect_s3_class(plastic_proportions(countries = NULL, years = NULL),
                            "gt_tbl")

  dat_clean <- auditr:::clean_data()

  result <- plastic_proportions(countries = c("Argentina", "Canada"),
                                years = c(2019, 2020))

  testthat::expect_s3_class(result, "gt_tbl")


  result_data <- result$`_data`


  testthat::expect_equal(names(result_data), c("country", "year",
                                               "empty", "hdpe",	"ldpe",	"o",
                                               "pet",	"pp",	"ps",	"pvc"))

  testthat::expect_equal(round(result_data$hdpe[1], 2), 0.08)
  testthat::expect_equal(result_data$country[3], "Canada")
  testthat::expect_equal(round(result_data$o[4], 2), 0.23)

})



testthat::test_that("input validation works", {

  testthat::expect_error(
    plastic_proportions(countries = 123, years = 2019))

  testthat::expect_error(
    plastic_proportions(countries = "Canada", years = TRUE))


  testthat::expect_error(
    plastic_proportions(countries = character(0), years = 2019))

  testthat::expect_error(
    plastic_proportions(countries = "Canada", years = numeric(0)))


  testthat::expect_warning(
    plastic_proportions(countries = "Fakecountry", years = 2019))

})
