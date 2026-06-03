testthat::test_that("rateify creates a tibble and tidy evaluation works", {
  res <- rate_ify2(load_data(), numerators = ldpe:pvc, denominator = grand_total)
  testthat::expect_s3_class(res, "data.frame")

})


testthat::test_that("rate_ify2 does not change other columns", {
  data <- load_data()
  res <- rate_ify2(df = data, numerators = ldpe:pvc, denominator = grand_total)
  testthat::expect_equal(data$grand_total, res$grand_total)
  testthat::expect_equal(data$volunteers, res$volunteers)
})

testthat::test_that("rate_ify returns a tibble of the same size and the keeps column names intact",{
  data <- load_data()
  res <- rate_ify2(df = data, numerators = ldpe:pvc, denominator = grand_total)
  testthat::expect_equal(nrow(data), nrow(res))
  testthat::expect_equal(colnames(data), colnames(res))
})
