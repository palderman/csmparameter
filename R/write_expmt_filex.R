#' @importFrom dplyr "%>%" group_by group_walk
#' @importFrom DSSAT write_filex
#'
write_exmpt_filex <- function(.expmt_tbl){

  .expmt_tbl %>%
    group_by(filex_name) %>%
    group_walk(~{
      if('VBOSE' %in% colnames(.x$filex[[1]]$`SIMULATION CONTROLS`)){
        .x$filex[[1]]$`SIMULATION CONTROLS`$VBOSE <- 'N'
      }else if('LONG' %in% colnames(.x$filex[[1]]$`SIMULATION CONTROLS`)){
        .x$filex[[1]]$`SIMULATION CONTROLS`$LONG <- 'N'
      }
      write_filex(.x$filex[[1]],.y$filex_name)})

}
