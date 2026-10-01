standardize_column_order <- function(df){
  if("prm_df" %in% class(df)){
    cnames <- c("pname", "pnum", "pfmt", "psampler", "pdensity",
                "ptransform",
                colnames(df)) |>
      unique()
    cnames <- cnames[cnames %in% colnames(df)]
  }else{
    cnames <- colnames(df)
  }
  df[, cnames]
}
