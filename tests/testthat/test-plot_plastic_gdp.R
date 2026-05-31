test_that("output creates a plot", {
  df <- load_full_data()
  density_plot1 <- density_ploter(data = df,
                                  selected_year = 2020,
                                  cutoff = 2000,
                                  chosen_types = c("hdpe", "pvc", "other"))

  expect_s3_class(density_plot1, "ggplot")
})
