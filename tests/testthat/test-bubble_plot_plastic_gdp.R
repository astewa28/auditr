
testthat::test_that("output creates a plot", {

  plot1 <- bubble_plotter(countries = c("China", "Canada", "Nigeria", "France"))

  plot2 <- bubble_plotter(countries = c("china", "argentina"))

  testthat::expect_s3_class(plot1, "ggplot")
  testthat::expect_s3_class(plot2, "ggplot")

})


testthat::test_that("bubble_plotter works with different inputs", {

  bubble_plot <-
  testthat::expect_s3_class(density_plot2, "ggplot")
})


testthat::test_that("density_ploter rejects invalid year", {
  data(load_data(), package = "auditr", envir = environment())
  testthat::expect_error(
    auditr::density_ploter(
      data = load_data(),
      selected_year = 2021,
      cutoff = 2000,
      chosen_types = c("hdpe", "pvc", "other")
    ),
    "year must be either 2019 or 2020"
  )
})
