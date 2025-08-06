library(tinytest)

prm_df <- csmparameter::prm_create_prm_df(pname = "G1",
                                           pfile = "WHCER048.CUL")

new_df <-
  csmparameter::prm_add_variances(prm_df,
                                  psigma = c(1000, 0.1, 1000, 0.1),
                                  EXPERIMENT = "INST0001",
                                  TRNO = rep(1:2, each = 2),
                                  DATE = as.POSIXct("0001001",
                                                    format = "%Y%j"),
                                  variable = rep(c("HWAM", "HIAM"), 2))

expect_identical(new_df[["pname"]],
                 c("G1",
                   "sigma_r;EXPERIMENT:INST0001;TRNO:1;DATE:1-01-01;variable:HWAM",
                   "sigma_r;EXPERIMENT:INST0001;TRNO:1;DATE:1-01-01;variable:HIAM",
                   "sigma_r;EXPERIMENT:INST0001;TRNO:2;DATE:1-01-01;variable:HWAM",
                   "sigma_r;EXPERIMENT:INST0001;TRNO:2;DATE:1-01-01;variable:HIAM"),
                 info = "pname")

expect_identical(new_df[["pfile"]],
                 c("WHCER048.CUL", rep("", 4)),
                 info = "pfile")

density_1000 <- csmparameter:::normal_prior_density(0, Inf, 0, 1000)
density_pt1 <- csmparameter:::normal_prior_density(0, Inf, 0, 0.1)

expect_identical(sapply(new_df[["pdensity"]],function(x) x(0.)),
                 c(prm_df$pdensity[[1]](0.),
                   rep(c(density_1000(0.),
                         density_pt1(0.)), 2)),
                 info = "pdensity 0")

expect_identical(sapply(new_df[["pdensity"]],function(x) x(1.)),
                 c(prm_df$pdensity[[1]](1.),
                   rep(c(density_1000(1.),
                         density_pt1(1.)), 2)),
                 info = "pdensity ")

expect_identical(sapply(new_df[["pdensity"]],function(x) x(3.)),
                 c(prm_df$pdensity[[1]](3.),
                   rep(c(density_1000(3.),
                         density_pt1(3.)), 2)))
