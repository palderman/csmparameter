test_that("lp for pmu", {

  obs_tbl <- NULL

  sim_tbl <- NULL

  pmu <- c(25, 32, 3.8)

  prm_tbl <- parametR::prm_create(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001")

  prior_lp <- 0
  for(i in seq_along(prm_tbl$pdensity)){
    prior_lp <- prior_lp + prm_tbl$pdensity[[i]](pmu[i])
  }

  expect_identical(
    parametR::lp_fun(obs_tbl, sim_tbl, prm_tbl, pmu),
    prior_lp
  )

})
