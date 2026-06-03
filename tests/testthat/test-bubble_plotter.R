
testthat::test_that("output creates a plot", {

  plot1 <- bubble_plotter(countries = c("China", "Canada", "Nigeria", "France"))

  plot2 <- bubble_plotter(countries = c("china", "argentina"))

  testthat::expect_s3_class(plot1, "ggplot")
  testthat::expect_s3_class(plot2, "ggplot")

})
