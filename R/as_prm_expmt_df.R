#' @export
#'
as_prm_expmt_df <- function(df_in){
  UseMethod("as_prm_expmt_df")
}

#' @export
#'
as_prm_expmt_df.default <- function(df_in){
  if(class(df_in)[1] != 'prm_expmt_df'){
    df_out <- df_in
    class(df_out) <- c('prm_expmt_df', class(df_in))
  }else{
    df_out <- df_in
  }
  return(df_out)
}
