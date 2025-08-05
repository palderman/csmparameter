
  obs_tbl <- tidyr::unnest(
    dplyr::mutate(
      dplyr::group_by(
        tibble::tribble(
          ~EXPERIMENT, ~TRNO,                    ~DATE, ~variable, ~obs, ~lp_sigma_ind,
          "INST0001",      1, as.POSIXct("0001-01-01"),    "HWAM", 4000,             4,
          "INST0001",      1, as.POSIXct("0001-01-01"),    "HIAM", 0.35,             5,
          "INST0001",      2, as.POSIXct("0001-01-01"),    "HWAM", 4000,             4,
          "INST0001",      2, as.POSIXct("0001-01-01"),    "HIAM", 0.35,             5
        ),
        EXPERIMENT, TRNO, DATE, variable
      ),
      obs = list(rnorm(4, mean = obs, sd = obs*.13))
    ),
    obs
  )

  sim_tbl <- dplyr::summarize(obs_tbl, sim = mean(obs))

  pmu <- c(25, 32, 3.8)

  prm_tbl <- parametR::prm_add_variances(
    parametR::prm_create(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001"),
    dplyr::group_by(obs_tbl, variable),
    psigma = c(250, 0.025),
    variable = c("HWAM", "HIAM")
  )

  pval <- c(pmu, 250, 0.025)
  prior_lp <- sum(
    sapply(
      seq_along(prm_tbl$pdensity),
      function(i) prm_tbl$pdensity[[i]](pval[i])
      )
    )

  sd_tmp <- sapply(obs_tbl$variable, switch, "HWAM" = 250, "HIAM" = 0.025)

  log_likelihood <- with(dplyr::full_join(obs_tbl, sim_tbl),
                         sum(
                           dnorm(obs,
                                 mean = sim,
                                 sd = sd_tmp,
                                 log = TRUE)
                           )
                         )

  lp <- prior_lp + log_likelihood

test_that("lp_prior_density()", {
  expect_identical(
    parametR:::lp_prior_density(prm_tbl, pval),
    prior_lp
  )
})

test_that("lp_fun()", {

  expect_identical(
    parametR::lp_fun(obs_tbl, sim_tbl, prm_tbl, pval),
    lp
  )

})
