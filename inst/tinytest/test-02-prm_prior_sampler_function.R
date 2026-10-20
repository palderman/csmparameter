library(tinytest)

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = -Inf,
                                                      pmax = Inf,
                                                      pmu = 0,
                                                      psigma = 1)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
expected <- qnorm(runif(7))

expect_identical(actual,
                 expected,
                 info = "normal  min = -Inf; max = Inf; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = 0,
                                                      pmax = Inf,
                                                      pmu = 0,
                                                      psigma = 1)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
prob_pmin <- pnorm(0, mean = 0, sd = 1)
prob_pmax <- pnorm(Inf, mean = 0, sd = 1)

expected <- qnorm(
  exp(log(runif(7))+log(prob_pmax - prob_pmin))+prob_pmin,
  mean = 0,
  sd = 1)

expect_identical(actual, expected,
                 info = "normal  min = 0; max = Inf; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = -Inf,
                                                      pmax = 0,
                                                      pmu = 0,
                                                      psigma = 1)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
prob_pmin <- pnorm(-Inf, mean = 0, sd = 1)
prob_pmax <- pnorm(0, mean = 0, sd = 1)
expected <- qnorm(
  exp(log(runif(7))+log(prob_pmax - prob_pmin))+prob_pmin,
  mean = 0,
  sd = 1)

expect_identical(actual, expected,
                 info = "normal  min = -Inf; max = 0; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = 0,
                                                      pmax = NA,
                                                      pmu = 0,
                                                      psigma = 1)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
prob_pmin <- pnorm(0, mean = 0, sd = 1)
prob_pmax <- pnorm(Inf, mean = 0, sd = 1)

expected <- qnorm(
  exp(log(runif(7))+log(prob_pmax - prob_pmin))+prob_pmin,
  mean = 0,
  sd = 1)

expect_identical(actual, expected,
                 info = "normal  min = 0; max = NA; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = 0,
                                                      pmax = NULL,
                                                      pmu = 0,
                                                      psigma = 1)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
prob_pmin <- pnorm(0, mean = 0, sd = 1)
prob_pmax <- pnorm(Inf, mean = 0, sd = 1)
expected <- qnorm(
  exp(log(runif(7))+log(prob_pmax - prob_pmin))+prob_pmin,
  mean = 0,
  sd = 1)

expect_identical(actual, expected,
                 info = "normal  min = 0; max = NULL; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = NULL,
                                                      pmax = 0,
                                                      pmu = 0,
                                                      psigma = 1)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
prob_pmin <- pnorm(-Inf, mean = 0, sd = 1)
prob_pmax <- pnorm(0, mean = 0, sd = 1)
expected <- qnorm(
  exp(log(runif(7))+log(prob_pmax - prob_pmin))+prob_pmin,
  mean = 0,
  sd = 1)

expect_identical(actual, expected,
                 info = "normal  min = NULL; max = 0; mu = 0; sigma = 1")

pmin = 1
pmax = 3
pmu = 2
psigma = 1

prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "normal",
                                                      pmin = pmin,
                                                      pmax = pmax,
                                                      pmu = pmu,
                                                      psigma = psigma)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
prob_pmin <- pnorm(pmin, mean = pmu, sd = psigma)
prob_pmax <- pnorm(pmax, mean = pmu, sd = psigma)
expected <- qnorm(
  exp(log(runif(7))+log(prob_pmax - prob_pmin))+prob_pmin,
  mean = pmu,
  sd = psigma)

expect_identical(actual, expected,
                 info = "normal  min = 1; max = 3; mu = 2; sigma = 1")

pmin = 1
pmax = 3
pmu = NA
psigma = NA


prior_fun <- csmparameter::prm_prior_sampler_function(pdist = "uniform",
                                                      pmin = pmin,
                                                      pmax = pmax)

set.seed(1234)
actual <- prior_fun(7)

set.seed(1234)
expected <- exp(log(runif(7))+log((pmax - pmin))) + pmin

expect_identical(actual, expected,
                 info = "uniform  min = 1; max = 3; mu = NA; sigma = NA")
