#'
prm_from_eco_file <- function(file_path){

  eco <- DSSAT::read_eco(file_path) |>
    subset(`ECO#` %in% c("999991", "999992", "DFAULT"))

  eco_df <-
    eco |>
    subset(select = -`ECO#`) |>
    t() |>
    as.data.frame() |>
    setNames(eco[["ECO#"]]) |>
    df_rename(pmin = "999991",
              pmax = "999992",
              pmu = "DFAULT")

  eco_df[["pname"]] <- rownames(eco_df)

  eco_df |>
    within({
      psigma = (pmax-pmin)/4
      pfile = basename(file_path)
    }) |>
    with(prm_create_prm_df(pname = pname,
                           pfile = pfile,
                           pmin = pmin,
                           pmax = pmax,
                           pmu = pmu,
                           psigma = psigma,
                           pdist = "normal"))

}
