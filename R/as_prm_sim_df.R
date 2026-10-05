#' @export
#'
as_prm_sim_df <- function(df_in){
  UseMethod("as_prm_sim_df")
}

#' @export
#'
as_prm_sim_df.default <- function(df_in){
  if(class(df_in)[1] != 'prm_sim_df'){
    df_out <- df_in
    class(df_out) <- c('prm_sim_df', class(df_in))
  }else{
    df_out <- df_in
  }
  return(df_out)
}
