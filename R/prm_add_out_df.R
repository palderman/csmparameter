#' Add output data frame
#'
#' @param .expmt a data frame with information about experiments to be used for
#'  estimation
#'
#' @param prioritize_files a character vector of files to prioritize in
#'  searching for output variables
#'
prm_add_out_df <- function(.expmt, prioritize_files = c('Summary.OUT',
                                                        'PlantGro.OUT')){

  DSSAT::write_dssbatch(x=.expmt$filex_name,
                        trtno=.expmt$trno[[1]])

  DSSAT::run_dssat(suppress_output = TRUE)

  .expmt <-
    .expmt |>
    by(list(.expmt$filex_name),
       \(.x){
         within(.x,{
           out_df = list(find_output_variables(.x, prioritize_files))
         })
       }) |>
    do.call(rbind, args = _)

  return(.expmt)

}
