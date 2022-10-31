test_that("single sample no NULL", {

  prm_tbl <- parametR::prm_create(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001")

  set.seed(1234)
  expected <- matrix(
    sapply(prm_tbl$psampler, function(f) f(1)),
    nrow = 1)

  set.seed(1234)
  actual <- parametR::prm_sample_prior(prm_tbl)

  expect_identical(actual, expected)

})

test_that("single sample with NULL", {

  prm_tbl <- parametR::prm_create(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001")

  prm_tbl$psampler[2] <- list(NULL)

  set.seed(1234)
  expected <- matrix(
    sapply(prm_tbl$psampler[c(1,3)], function(f) f(1)),
    nrow = 1)

  set.seed(1234)
  actual <- parametR::prm_sample_prior(prm_tbl)

  expect_identical(actual, expected)

})

test_that("five samples no NULL", {

  prm_tbl <- parametR::prm_create(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001")

  set.seed(1234)
  expected <- do.call(cbind,
    lapply(prm_tbl$psampler, function(f) f(5)))

  set.seed(1234)
  actual <- parametR::prm_sample_prior(prm_tbl, 5)

  expect_identical(actual, expected)

})

test_that("five sample with NULL", {

  prm_tbl <- parametR::prm_create(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001")

  prm_tbl$psampler[2] <- list(NULL)

  set.seed(1234)
  expected <- do.call(cbind,
    lapply(prm_tbl$psampler[c(1,3)], function(f) f(5)))

  set.seed(1234)
  actual <- parametR::prm_sample_prior(prm_tbl, 5)

  expect_identical(actual, expected)

})
