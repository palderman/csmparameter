#'
#' @importFrom readr read_csv
#' @importFrom dplyr  pull
#'
#' @export
#'
prm_import_prm_df <- function(file_name){

  prm_df_csv <- read_csv(file_name)

  pname <- prm_df_csv |>
    pull(pname) |>
    as.character()

  pfile <- prm_df_csv |>
    pull(pfile) |>
    as.character()

  if('pmin' %in% colnames(prm_df_csv)){
    pmin <- prm_df_csv |>
      pull(pmin) |>
      as.numeric()
  }else{
    pmin <- -Inf
  }

  if('pmax' %in% colnames(prm_df_csv)){
    pmax <- prm_df_csv |>
      pull(pmax) |>
      as.numeric()
  }else{
    pmax <- Inf
  }

  if('pmu' %in% colnames(prm_df_csv)){
    pmu <- prm_df_csv |>
      pull(pmu) |>
      as.numeric()
  }else{
    pmu <- NA
  }

  if('psigma' %in% colnames(prm_df_csv)){
    psigma <- prm_df_csv |>
      pull(psigma) |>
      as.numeric()
  }else{
    psigma <- NA
  }

  if('pdist' %in% colnames(prm_df_csv)){
    pdist <- prm_df_csv |>
      pull(pdist) |>
      as.character()
  }else{
    pdist <- "unif"
  }

  if('ptier' %in% colnames(prm_df_csv)){
    ptier <- prm_df_csv |>
      pull(ptier) |>
      as.character()
  }else{
    ptier <- as.character(NA)
  }

  if('pkey' %in% colnames(prm_df_csv)){
    pkey <- prm_df_csv |>
      pull(pkey) |>
      as.character()
  }else{
    pkey <- as.character(NA)
  }

  if('plev' %in% colnames(prm_df_csv)){
    plev <- prm_df_csv |>
      pull(plev) |>
      as.numeric()
  }else{
    plev <- as.numeric(NA)
  }

  if('pind' %in% colnames(prm_df_csv)){
    pind <- prm_df_csv |>
      pull(pind) |>
      as.numeric()
  }else{
    pind <- as.numeric(NA)
  }

  if('pnum' %in% colnames(prm_df_csv)){
    pnum <- prm_df_csv |>
      pull(pnum) |>
      as.numeric()
  }else{
    pnum <- NULL
  }

  if('pwt' %in% colnames(prm_df_csv)){
    pwt <- prm_df_csv |>
      pull(pwt) |>
      as.numeric()
  }else{
    pwt <- NULL
  }

  prm_df <- prm_create(pname = pname,
                        pfile = pfile,
                        pmin = pmin,
                        pmax = pmax,
                        pmu = pmu,
                        psigma = psigma,
                        pdist = pdist,
                        ptier = ptier,
                        pkey = pkey,
                        plev = plev,
                        pind = pind,
                        pnum = pnum,
                        pwt = pwt)

  return(prm_df)

}
