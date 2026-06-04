testthat::test_that("stat_analysis returns a gt table", {
  result <- stat_analysis()

  testthat::expect_s3_class(result, "gt_tbl")
})

testthat::test_that("stat_analysis works with a smaller set", {
  result <- stat_analysis(
    fixed_vars = c("Population", "gdp", "region")
  )

  testthat::expect_s3_class(result, "gt_tbl")
})

testthat::test_that("stat_analysis works", {
  result <- stat_analysis(
    fixed_vars = c("Population", "pop_density", "gdp")
  )

  testthat::expect_s3_class(result, "gt_tbl")
})

testthat::test_that("stat_analysis errors when fixed_vars is not character", {
  testthat::expect_error(
    stat_analysis(fixed_vars = 123),
    regexp = "fixed_vars must be a character vector"
  )
})

testthat::test_that("stat_analysis errors when fixed_vars is empty", {
  testthat::expect_error(
    stat_analysis(fixed_vars = character(0)),
    regexp = "fixed_vars must contain at least one predictor variable"
  )
})

testthat::test_that("stat_analysis errors when fixed_vars contains NA", {
  testthat::expect_error(
    stat_analysis(fixed_vars = c("Population", NA)),
    regexp = "fixed_vars cannot contain NA values"
  )
})

testthat::test_that("stat_analysis errors when fixed_vars contains empty string", {
  testthat::expect_error(
    stat_analysis(fixed_vars = c("Population", "")),
    regexp = "fixed_vars cannot contain empty strings"
  )
})

testthat::test_that("stat_analysis errors when fixed_vars contains an invalid variable", {
  testthat::expect_error(
    stat_analysis(fixed_vars = c("Population", "fake_variable")),
    regexp = "Invalid fixed_vars: fake_variable"
  )
})
