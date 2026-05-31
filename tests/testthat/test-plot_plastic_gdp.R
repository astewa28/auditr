testthat::test_that("density_ploter works", {

  test_data <- tibble::tibble(
    country = c("A", "A", "B", "B", "C", "C"),
    year = c(2019, 2019, 2019, 2019, 2020, 2020),
    empty = c(10, 20, 30, 40, 50, 60),
    hdpe = c(5, 10, 15, 20, 25, 30),
    ldpe = c(2, 4, 6, 8, 10, 12),
    o = c(1, 2, 3, 4, 5, 6),
    pet = c(7, 8, 9, 10, 11, 12),
    pp = c(3, 4, 5, 6, 7, 8),
    ps = c(1, 1, 2, 2, 3, 3),
    pvc = c(0, 1, 0, 1, 0, 1),
    total = c(29, 50, 70, 91, 111, 132),
    gdp_per_capita = c(1000, 1000, 15000, 15000, 8000, 8000),
    HDI = c(0.5, 0.5, 0.9, 0.9, 0.7, 0.7)
  )

  result <- auditr::density_ploter(
    data = test_data,
    selected_year = 2019,
    cutoff = 100,
    chosen_types = c("hdpe", "pvc", "other")
  )

  testthat::expect_s3_class(result, "ggplot")
})


testthat::test_that("density_ploter validates selected plastic types", {

  test_data <- tibble::tibble(
    country = "A",
    year = 2019,
    empty = 10,
    hdpe = 5,
    ldpe = 2,
    o = 1,
    pet = 7,
    pp = 3,
    ps = 1,
    pvc = 0,
    total = 29,
    gdp_per_capita = 1000,
    HDI = 0.5
  )

  testthat::expect_error(
    auditr::density_ploter(
      data = test_data,
      chosen_types = "glass"
    ),
    "plastic_type is not in dataframe"
  )
})


testthat::test_that("density_ploter validates selected year", {

  test_data <- tibble::tibble(
    country = "A",
    year = 2019,
    empty = 10,
    hdpe = 5,
    ldpe = 2,
    o = 1,
    pet = 7,
    pp = 3,
    ps = 1,
    pvc = 0,
    total = 29,
    gdp_per_capita = 1000,
    HDI = 0.5
  )

  testthat::expect_error(
    auditr::density_ploter(
      data = test_data,
      selected_year = 2021
    ),
    "year must be either 2019 or 2020"
  )
})
