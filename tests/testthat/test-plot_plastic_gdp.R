testthat::test_that("output creates a plot", {
  data("final_merged", package = "auditr", envir = environment())
  density_plot1 <- auditr::density_ploter(
    data = final_merged,
    selected_year = 2020,
    cutoff = 2000,
    chosen_types = c("hdpe", "pvc", "other")
  )
  testthat::expect_s3_class(density_plot1, "ggplot")
})


testthat::test_that("density_ploter works with different inputs", {
  data("final_merged", package = "auditr", envir = environment())
  density_plot2 <- auditr::density_ploter(
    data = final_merged,
    selected_year = 2019,
    cutoff = 1000,
    chosen_types = c("pet", "pp", "ps")
  )
  testthat::expect_s3_class(density_plot2, "ggplot")
})


testthat::test_that("density_ploter rejects invalid year", {
  data("final_merged", package = "auditr", envir = environment())
  testthat::expect_error(
    auditr::density_ploter(
      data = final_merged,
      selected_year = 2021,
      cutoff = 2000,
      chosen_types = c("hdpe", "pvc", "other")
    ),
    "year must be either 2019 or 2020"
  )
})
