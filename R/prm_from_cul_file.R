#'
prm_from_cul_file <- function(file_path){

  cul <- DSSAT::read_cul(file_path) |>
    subset(`VAR-NAME` %in% c("MINIMA", "MAXIMA", "DEFAULT"),
           select = -c(`VAR#`, `EXP#`, `ECO#`))

  cul_df <-
    cul |>
    subset(select = -`VAR-NAME`) |>
    t() |>
    as.data.frame() |>
    setNames(cul[["VAR-NAME"]]) |>
    df_rename(pmin = "MINIMA",
              pmax = "MAXIMA",
              pmu = "DFAULT")

  cul_df[["pname"]] <- rownames(cul_df)


  cul_df |>
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
