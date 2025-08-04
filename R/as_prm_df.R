#' @export
as_prm_df <- function(df_in){
  UseMethod("as_prm_df")
}

#'
#' @importFrom tibble as_tibble
#'
#' @export
#'
as_prm_df.default <- function(df_in){

  if(class(df_in)[1] != 'prm_df'){

    df_out <- as_tibble(df_in)
    class(df_out) <- c('prm_df', class(df_in))

  }else{

    df_out <- df_in

  }
  return(df_out)
}
