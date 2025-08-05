library(tinytest)

expect_equal(csmparameter:::tnorm_dens_adj(-Inf, Inf, 0, 1),
             log(1),
             info = "tnorm_dens_adj min = -Inf; max = Inf; mu = 0; sigma = 1")

expect_equal(csmparameter:::tnorm_dens_adj(0, Inf, 0, 1),
             log(0.5),
             info = "tnorm_dens_adj min = 0; max = Inf; mu = 0; sigma = 1")

expect_equal(csmparameter:::tnorm_dens_adj(-Inf, 0, 0, 1),
             log(0.5),
             info = "tnorm_dens_adj min = -Inf; max = 0; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(-Inf, Inf, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 dnorm(-3:3, log = TRUE),
                 info = "normal  min = -Inf; max = Inf; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(0, Inf, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)),
                 info = "normal  min = 0; max = Inf; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(-Inf, 0, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 c(dnorm(-3:-1, log = TRUE)-log(0.5), rep(-Inf, 4)),
                 info = "normal  min = -Inf; max = 0; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(0, NA, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)),
                 info = "normal  min = 0; max = NA; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(NA, 0, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 c(dnorm(-3:-1, log = TRUE)-log(0.5), rep(-Inf, 4)),
                 info = "normal  min = NA; max = 0; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(0, NULL, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 c(rep(-Inf, 4), dnorm(1:3, log = TRUE) - log(0.5)),
                 info = "normal  min = 0; max = NULL; mu = 0; sigma = 1")

prior_fun <- csmparameter::prm_prior_density_function(NULL, 0, 0, 1, "normal")

expect_identical(sapply(-3:3, prior_fun),
                 c(dnorm(-3:-1, log = TRUE)-log(0.5), rep(-Inf, 4)),
                 info = "normal  min = NULL; max = 0; mu = 0; sigma = 1")

pmin = 1
pmax = 3
pmu = 2
psigma = 1

prior_fun <- csmparameter::prm_prior_density_function(pmin, pmax, pmu, psigma, "normal")

x <- csmparameter:::tnorm_test_vector(pmin, pmax, pmu, psigma)

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
                 expected,
                 info = "normal  min = 1; max = 3; mu = 2; sigma = 1")

pmin = 1
pmax = 3
pmu = NA
psigma = NA

prior_fun <- csmparameter::prm_prior_density_function(pmin, pmax, pmu, psigma, "uniform")

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
                 expected,
                 info = "uniform  min = 1; max = 3; mu = NA; sigma = NA")

