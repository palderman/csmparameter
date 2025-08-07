#'
#' @export
#'
prm_import_prm_df <- function(file_name){

  prm_df_csv <- read.csv(file_name)

  pname <- prm_df_csv[["pname"]] |>
    as.character()

  pfile <- prm_df_csv[["pfile"]] |>
    as.character()

  if('pmin' %in% colnames(prm_df_csv)){
    pmin <- prm_df_csv[["pmin"]] |>
      as.numeric()
  }else{
    pmin <- -Inf
  }

  if('pmax' %in% colnames(prm_df_csv)){
    pmax <- prm_df_csv[["pmax"]] |>
      as.numeric()
  }else{
    pmax <- Inf
  }

  if('pmu' %in% colnames(prm_df_csv)){
    pmu <- prm_df_csv[["pmu"]] |>
      as.numeric()
  }else{
    pmu <- NA
  }

  if('psigma' %in% colnames(prm_df_csv)){
    psigma <- prm_df_csv[["psigma"]] |>
      as.numeric()
  }else{
    psigma <- NA
  }

  if('pdist' %in% colnames(prm_df_csv)){
    pdist <- prm_df_csv[["pdist"]] |>
      as.character()
  }else{
    pdist <- "unif"
  }

  if('ptier' %in% colnames(prm_df_csv)){
    ptier <- prm_df_csv[["ptier"]] |>
      as.character()
  }else{
    ptier <- NA_character_
  }

  if('pkey' %in% colnames(prm_df_csv)){
    pkey <- prm_df_csv[["pkey"]] |>
      as.character()
  }else{
    pkey <- NA_character_
  }

  if('plev' %in% colnames(prm_df_csv)){
    plev <- prm_df_csv[["plev"]] |>
      as.numeric()
  }else{
    plev <- NA_real_
  }

  if('pind' %in% colnames(prm_df_csv)){
    pind <- prm_df_csv[["pind"]] |>
      as.numeric()
  }else{
    pind <- NA_real_
  }

  if('pnum' %in% colnames(prm_df_csv)){
    pnum <- prm_df_csv[["pnum"]] |>
      as.numeric()
  }else{
    pnum <- NULL
  }

  if('pwt' %in% colnames(prm_df_csv)){
    pwt <- prm_df_csv[["pwt"]] |>
      as.numeric()
  }else{
    pwt <- NULL
  }

  prm_df <- prm_create_prm_df(pname = pname,
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
