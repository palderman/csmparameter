prm_df <- csmparameter::prm_create_prm_df(pname = c("G1", "G2", "G3"),
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
  sapply(prm_df$psampler, function(f) f(1)),
  nrow = 1)

set.seed(1234)
actual <- csmparameter::prm_sample_prior(prm_df)

expect_identical(actual, expected,
                 info = "single sample no NULL")

prm_df <- csmparameter::prm_create_prm_df(pname = c("G1", "G2", "G3"),
                                          pmin = c(16, 1.2, 0.5),
                                          pmax = c(36, 43, 23),
                                          pmu = c(25, 32, 3.8),
                                          psigma = c(5, 11, 6.2),
                                          pdist = "normal",
                                          pfile = "WHCER048.CUL",
                                          ptier = NA,
                                          pkey = "IB0001")

prm_df$psampler[2] <- list(NULL)

set.seed(1234)
expected <- matrix(
  sapply(prm_df$psampler[c(1,3)], function(f) f(1)),
  nrow = 1)

set.seed(1234)
actual <- csmparameter::prm_sample_prior(prm_df)

expect_identical(actual, expected,
                 info = "single sample with NULL")

prm_df <- csmparameter::prm_create_prm_df(pname = c("G1", "G2", "G3"),
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
                    lapply(prm_df$psampler, function(f) f(5)))

set.seed(1234)
actual <- csmparameter::prm_sample_prior(prm_df, 5)

expect_identical(actual, expected,
                 info = "five samples no NULL")

prm_df <- csmparameter::prm_create_prm_df(pname = c("G1", "G2", "G3"),
                                          pmin = c(16, 1.2, 0.5),
                                          pmax = c(36, 43, 23),
                                          pmu = c(25, 32, 3.8),
                                          psigma = c(5, 11, 6.2),
                                          pdist = "normal",
                                          pfile = "WHCER048.CUL",
                                          ptier = NA,
                                          pkey = "IB0001")

prm_df$psampler[2] <- list(NULL)

set.seed(1234)
expected <- do.call(cbind,
                    lapply(prm_df$psampler[c(1,3)], function(f) f(5)))

set.seed(1234)
actual <- csmparameter::prm_sample_prior(prm_df, 5)

expect_identical(actual, expected,
                 info = "five sample with NULL")
