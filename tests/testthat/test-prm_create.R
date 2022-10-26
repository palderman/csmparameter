test_that("single parameter full specification", {

  actual <- parametR::prm_create(pname = "G1",
                                 pmin = 16,
                                 pmax = 36,
                                 pmu = 25,
                                 psigma = 5,
                                 pdist = "normal",
                                 pfile = "WHCER048.CUL",
                                 ptier = NA,
                                 pkey = "IB0001")

    expect_identical(getElement(actual,"pname"),
                     "G1")
    expect_identical(getElement(actual,"pmin"),
                     NULL)
    expect_identical(getElement(actual,"pmax"),
                     NULL)
    expect_identical(getElement(actual,"pmu"),
                     NULL)
    expect_identical(getElement(actual,"psigma"),
                     NULL)
    expect_identical(getElement(actual,"pdist"),
                     NULL)
    expect_identical(getElement(actual,"pfile"),
                     "WHCER048.CUL")
    expect_identical(getElement(actual,"ptier"),
                     NA_character_)
    expect_identical(getElement(actual,"pkey"),
                     "IB0001")
    expect_identical(getElement(actual,"plev"),
                     NA_integer_)
    expect_identical(getElement(actual,"pind"),
                     NA_integer_)
    expect_identical(getElement(actual,"pnum"),
                     1L)
    expect_identical(getElement(actual, "pdensity"),
                     list(parametR:::prm_prior_density(
                       pmin = 16,
                       pmax = 36,
                       pmu = 25,
                       psigma = 5,
                       pdist = "normal"
                     )),
                     ignore_function_env = TRUE)

})

test_that("three parameter full specification", {

  actual <- parametR::prm_create(pname = c("G1", "G2", "G3"),
                                 pmin = c(16, 1.2, 0.5),
                                 pmax = c(36, 43, 23),
                                 pmu = c(25, 32, 3.8),
                                 psigma = c(5, 11, 6.2),
                                 pdist = "normal",
                                 pfile = "WHCER048.CUL",
                                 ptier = NA,
                                 pkey = "IB0001")

  expect_identical(getElement(actual,"pname"),
                   c("G1", "G2", "G3"))
  expect_identical(getElement(actual,"pmin"),
                   NULL)
  expect_identical(getElement(actual,"pmax"),
                   NULL)
  expect_identical(getElement(actual,"pmu"),
                   NULL)
  expect_identical(getElement(actual,"psigma"),
                   NULL)
  expect_identical(getElement(actual,"pdist"),
                   NULL)
  expect_identical(getElement(actual,"pfile"),
                   rep("WHCER048.CUL", 3))
  expect_identical(getElement(actual,"ptier"),
                   rep(NA_character_, 3))
  expect_identical(getElement(actual,"pkey"),
                   rep("IB0001", 3))
  expect_identical(getElement(actual,"plev"),
                   rep(NA_integer_, 3))
  expect_identical(getElement(actual,"pind"),
                   rep(NA_integer_, 3))
  expect_identical(getElement(actual,"pnum"),
                   1L:3L)
  expect_identical(getElement(actual, "pdensity"),
                   list(
                     parametR:::prm_prior_density(
                       pmin = 16,
                       pmax = 36,
                       pmu = 25,
                       psigma = 5,
                       pdist = "normal"),
                     parametR:::prm_prior_density(
                       pmin = 1.2,
                       pmax = 43,
                       pmu = 32,
                       psigma = 11,
                       pdist = "normal"),
                     parametR:::prm_prior_density(
                       pmin = 0.5,
                       pmax = 23,
                       pmu = 3.8,
                       psigma = 6.2,
                       pdist = "normal")
                   ),
                   ignore_function_env = TRUE)

})
