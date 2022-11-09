
prm_tbl <- parametR::prm_create("G1", "WHCER048.CUL")

obs_tbl <- dplyr::group_by(
  tibble::tribble(
    ~EXPERIMENT, ~TRNO,                    ~DATE, ~variable, ~obs,
    "INST0001",      1, as.POSIXct("0001-01-01"),    "HWAM", 4000,
    "INST0001",      1, as.POSIXct("0001-01-01"),    "HIAM", 0.35,
    "INST0001",      2, as.POSIXct("0001-01-01"),    "HWAM", 4000,
    "INST0001",      2, as.POSIXct("0001-01-01"),    "HIAM", 0.35
  ),
  EXPERIMENT, TRNO, DATE, variable
)

new_tbl <- parametR::prm_add_variances(prm_tbl, obs_tbl,
                                       psigma = c(1000, 0.1, 1000, 0.1),
                                       EXPERIMENT = "INST0001",
                                       TRNO = rep(1:2, each = 2),
                                       variable = rep(c("HIAM", "HWAM"), 2))

test_that("pname", {

    expect_identical(new_tbl[["pname"]],
                   c("G1",
                     "variance;EXPERIMENT:INST0001;TRNO:1;DATE:1-001;variable:HIAM",
                     "variance;EXPERIMENT:INST0001;TRNO:1;DATE:1-001;variable:HWAM",
                     "variance;EXPERIMENT:INST0001;TRNO:2;DATE:1-001;variable:HIAM",
                     "variance;EXPERIMENT:INST0001;TRNO:2;DATE:1-001;variable:HWAM"
                   ))

})

test_that("pfile", {

  expect_identical(new_tbl[["pfile"]],
                   c("WHCER048.CUL", rep("", 4)))

})

test_that("pdensity", {

  density_1000 <- parametR:::normal_prior_density(0, Inf, 0, 1000)
  density_pt1 <- parametR:::normal_prior_density(0, Inf, 0, 0.1)

  expect_identical(sapply(new_tbl[["pdensity"]],function(x) x(0.)),
                   c(prm_tbl$pdensity[[1]](0.),
                     rep(c(density_1000(0.),
                         density_pt1(0.)), 2)))

  expect_identical(sapply(new_tbl[["pdensity"]],function(x) x(1.)),
                   c(prm_tbl$pdensity[[1]](1.),
                     rep(c(density_1000(1.),
                         density_pt1(1.)), 2)))

  expect_identical(sapply(new_tbl[["pdensity"]],function(x) x(3.)),
                   c(prm_tbl$pdensity[[1]](3.),
                     rep(c(density_1000(3.),
                         density_pt1(3.)), 2)))
})

obs_tbl <- dplyr::group_by(obs_tbl, variable)

new_tbl <- parametR::prm_add_variances(prm_tbl, obs_tbl,
                                       psigma = c(1000, 0.1),
                                       variable = c("HWAM", "HIAM"))
