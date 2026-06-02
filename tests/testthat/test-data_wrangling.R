testthat::test_that("rateify creates a tibble", {
  res <- rate_ify2(load_data(), numerators = ldpe:pvc, denominator = grand_total)
  testthat::expect_s3_class(res, "tibble")

})
