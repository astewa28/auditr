testthat::test_that("load_data loads plastic pollution data", {
  dat <- auditr::load_data()

  testthat::expect_s3_class(dat, "data.frame")
  testthat::expect_equal(nrow(dat), 13380)
  testthat::expect_true(all(c("country", "year", "grand_total") %in% names(dat)))
})
