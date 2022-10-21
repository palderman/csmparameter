test_that("tnorm_dens_adj min = -Inf; max = Inf; mu = 0; sigma = 1", {
  expect_equal(parametR:::tnorm_dens_adj(-Inf, Inf, 0, 1),
               log(1))
})

test_that("tnorm_dens_adj min = 0; max = Inf; mu = 0; sigma = 1", {
  expect_equal(parametR:::tnorm_dens_adj(0, Inf, 0, 1),
               log(0.5))
})

test_that("tnorm_dens_adj min = -Inf; max = 0; mu = 0; sigma = 1", {
  expect_equal(parametR:::tnorm_dens_adj(-Inf, 0, 0, 1),
               log(0.5))
})

test_that("normal  min = -Inf; max = Inf; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(-Inf, Inf, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   dnorm(-3:3, log = TRUE))
})

test_that("normal  min = 0; max = Inf; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(0, Inf, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)))
})

test_that("normal  min = -Inf; max = 0; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(-Inf, 0, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(dnorm(-3:-1, log = TRUE)-log(0.5), rep(-Inf, 4)))
})

test_that("normal  min = 0; max = NA; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(0, NA, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)))
})

test_that("normal  min = NA; max = 0; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(NA, 0, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(dnorm(-3:-1, log = TRUE)-log(0.5), rep(-Inf, 4)))
})

test_that("normal  min = 0; max = NULL; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(0, NULL, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)))
})

test_that("normal  min = NULL; max = 0; mu = 0; sigma = 1", {
  prior_fun <- parametR::prm_prior_density(NULL, 0, 0, 1, "normal")

  expect_identical(sapply(-3:3, prior_fun),
                   c(dnorm(-3:-1, log = TRUE)-log(0.5), rep(-Inf, 4)))
})

test_that("normal  min = 1; max = 3; mu = 2; sigma = 1", {

  pmin = 1
  pmax = 3
  pmu = 2
  psigma = 1

  prior_fun <- parametR::prm_prior_density(pmin, pmax, pmu, psigma, "normal")

  x <- parametR:::tnorm_test_vector(pmin, pmax, pmu, psigma)

  expected <- sapply(x, function(v){
    if(v <= pmin | v >= pmax){
      return(-Inf)
    }else{
      return(dnorm(v, pmu, psigma, log = TRUE)-
               log(pnorm(pmax, pmu, psigma)-pnorm(pmin, pmu, psigma))
             )
    }
  })

  expect_identical(sapply(x, prior_fun),
                   expected)
})


test_that("uniform  min = 1; max = 3; mu = NA; sigma = NA", {

  pmin = 1
  pmax = 3
  pmu = NA
  psigma = NA

  prior_fun <- parametR::prm_prior_density(pmin, pmax, pmu, psigma, "uniform")

  prange <- pmax - pmin
  x <- seq(pmin - prange/2, pmax + prange/2, length.out = 9)

  expected <- sapply(x, function(v){
    if(v <= pmin | v >= pmax){
      return(-Inf)
    }else{
      return(dunif(v, pmin, pmax, log = TRUE))
    }
  })

  expect_identical(sapply(x, prior_fun),
                   expected)
})

