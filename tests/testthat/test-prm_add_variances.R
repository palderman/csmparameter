test_that("pname", {

  prm_tbl <- parametR::prm_create("G1", "WHCER048.CUL")

  obs_tbl <- dplyr::group_by(
    tibble::tribble(
      ~EXPERIMENT, ~TRNO,                    ~DATE, ~variable, ~obs,
       "INST0001",     1, as.POSIXct("0001-01-01"),    "HWAM", 4000,
       "INST0001",     1, as.POSIXct("0001-01-01"),    "HIAM", 0.35,
       "INST0001",     2, as.POSIXct("0001-01-01"),    "HWAM", 4000,
       "INST0001",     2, as.POSIXct("0001-01-01"),    "HIAM", 0.35
    ),
    EXPERIMENT, TRNO, DATE, variable
  )

  new_tbl <- parametR::prm_add_variances(prm_tbl, obs_tbl)

  expect_identical(new_tbl[["pname"]],
                   c("EXPERIMENT:INST0001;TRNO:1;DATE:1-001;variable:HIAM",
                     "EXPERIMENT:INST0001;TRNO:1;DATE:1-001;variable:HWAM",
                     "EXPERIMENT:INST0001;TRNO:2;DATE:1-001;variable:HIAM",
                     "EXPERIMENT:INST0001;TRNO:2;DATE:1-001;variable:HWAM"
                   ))

})
