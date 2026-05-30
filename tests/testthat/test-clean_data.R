

testthat::test_that("clean_data helper function works", {

  dat <- auditr:::clean_data()

  testthat::expect_s3_class(dat, "tbl")

  testthat::expect_equal(ncol(dat), 15)
  testthat::expect_equal(nrow(dat), 13380)
})
