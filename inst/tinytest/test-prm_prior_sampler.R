test_that("normal  min = -Inf; max = Inf; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_sampler(-Inf, Inf, 0, 1, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  expected <- qnorm(pnorm(rnorm(7)))

  expect_identical(actual, expected)
})

test_that("normal  min = 0; max = Inf; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_sampler(0, Inf, 0, 1, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  prob_pmin <- pnorm(0, mean = 0, sd = 1)
  prob_pmax <- pnorm(Inf, mean = 0, sd = 1)
  expected <- qnorm(
    exp(log(pnorm(rnorm(7)))+log(prob_pmax - prob_pmin))+prob_pmin,
    mean = 0,
    sd = 1)

  expect_identical(actual, expected)
})

test_that("normal  min = -Inf; max = 0; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_sampler(-Inf, 0, 0, 1, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  prob_pmin <- pnorm(-Inf, mean = 0, sd = 1)
  prob_pmax <- pnorm(0, mean = 0, sd = 1)
  expected <- qnorm(
    exp(log(pnorm(rnorm(7)))+log(prob_pmax - prob_pmin))+prob_pmin,
    mean = 0,
    sd = 1)

  expect_identical(actual, expected)
})

test_that("normal  min = 0; max = NA; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(0, NA, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)))
})

test_that("normal  min = 0; max = NA; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_sampler(0, NA, 0, 1, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  prob_pmin <- pnorm(0, mean = 0, sd = 1)
  prob_pmax <- pnorm(Inf, mean = 0, sd = 1)
  expected <- qnorm(
    exp(log(pnorm(rnorm(7)))+log(prob_pmax - prob_pmin))+prob_pmin,
    mean = 0,
    sd = 1)

  expect_identical(actual, expected)
})

test_that("normal  min = 0; max = NULL; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_sampler(0, NULL, 0, 1, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  prob_pmin <- pnorm(0, mean = 0, sd = 1)
  prob_pmax <- pnorm(Inf, mean = 0, sd = 1)
  expected <- qnorm(
    exp(log(pnorm(rnorm(7)))+log(prob_pmax - prob_pmin))+prob_pmin,
    mean = 0,
    sd = 1)

  expect_identical(actual, expected)
})

test_that("normal  min = NULL; max = 0; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_sampler(NULL, 0, 0, 1, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  prob_pmin <- pnorm(-Inf, mean = 0, sd = 1)
  prob_pmax <- pnorm(0, mean = 0, sd = 1)
  expected <- qnorm(
    exp(log(pnorm(rnorm(7)))+log(prob_pmax - prob_pmin))+prob_pmin,
    mean = 0,
    sd = 1)

  expect_identical(actual, expected)
})

test_that("normal  min = 1; max = 3; mu = 2; sigma = 1", {

  pmin = 1
  pmax = 3
  pmu = 2
  psigma = 1

  prior_fun <- parametR::prm_prior_sampler(pmin, pmax, pmu, psigma, "normal")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  prob_pmin <- pnorm(pmin, mean = pmu, sd = psigma)
  prob_pmax <- pnorm(pmax, mean = pmu, sd = psigma)
  expected <- qnorm(
    exp(log(pnorm(rnorm(7)))+log(prob_pmax - prob_pmin))+prob_pmin,
    mean = pmu,
    sd = psigma)

  expect_identical(actual, expected)
})


test_that("uniform  min = 1; max = 3; mu = NA; sigma = NA", {

  pmin = 1
  pmax = 3
  pmu = NA
  psigma = NA

  prior_fun <- parametR::prm_prior_sampler(pmin, pmax, pmu, psigma, "uniform")

  set.seed(1234)
  actual <- prior_fun(7)

  set.seed(1234)
  expected <- exp(log(pnorm(rnorm(7)))+log((pmax - pmin))) + pmin

  expect_identical(actual, expected)
})

