arrange_df <- function(df, by = NULL){
  if(is.null(by)) by <- colnames(df)
  df_out <- df[do.call(order,
                       df[by]), ]
  attr(df_out, "row.names") <- c(NA, nrow(df))
  return(df_out)
}
