library(tinytest)

# single parameter full specification

actual <- csmparameter::prm_create_prm_df(pname = "G1",
                                          pmin = 16,
                                          pmax = 36,
                                          pmu = 25,
                                          psigma = 5,
                                          pdist = "normal",
                                          pfile = "WHCER048.CUL",
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

expect_identical(getElement(actual,"pkey"),
                 "IB0001")

expect_equivalent(getElement(actual, "pdensity"),
                 list(csmparameter:::prm_prior_density_function(
                   pmin = 16,
                   pmax = 36,
                   pmu = 25,
                   psigma = 5,
                   pdist = "normal"
                 )))

expect_equivalent(getElement(actual, "psampler"),
                 list(csmparameter:::prm_prior_sampler_function(
                   pmin = 16,
                   pmax = 36,
                   pmu = 25,
                   psigma = 5,
                   pdist = "normal"
                 )))

# three parameter full specification

actual <- csmparameter::prm_create_prm_df(pname = c("G1", "G2", "G3"),
                                          pmin = c(16, 1.2, 0.5),
                                          pmax = c(36, 43, 23),
                                          pmu = c(25, 32, 3.8),
                                          psigma = c(5, 11, 6.2),
                                          pdist = "normal",
                                          pfile = "WHCER048.CUL",
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

expect_identical(getElement(actual,"pkey"),
                 rep("IB0001", 3))

expect_equivalent(getElement(actual, "pdensity"),
                 list(
                   csmparameter:::prm_prior_density_function(
                     pmin = 16,
                     pmax = 36,
                     pmu = 25,
                     psigma = 5,
                     pdist = "normal"),
                   csmparameter:::prm_prior_density_function(
                     pmin = 1.2,
                     pmax = 43,
                     pmu = 32,
                     psigma = 11,
                     pdist = "normal"),
                   csmparameter:::prm_prior_density_function(
                     pmin = 0.5,
                     pmax = 23,
                     pmu = 3.8,
                     psigma = 6.2,
                     pdist = "normal")
                 ))

expect_equivalent(getElement(actual, "psampler"),
                 list(
                   csmparameter:::prm_prior_sampler_function(
                     pmin = 16,
                     pmax = 36,
                     pmu = 25,
                     psigma = 5,
                     pdist = "normal"),
                   csmparameter:::prm_prior_sampler_function(
                     pmin = 1.2,
                     pmax = 43,
                     pmu = 32,
                     psigma = 11,
                     pdist = "normal"),
                   csmparameter:::prm_prior_sampler_function(
                     pmin = 0.5,
                     pmax = 23,
                     pmu = 3.8,
                     psigma = 6.2,
                     pdist = "normal")
                 ))

