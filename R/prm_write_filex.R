prm_write_filex <- function(.expmt_df){

  .expmt_df[["filex_name"]] |>
    lapply(\(.filex){
      .expmt_df |>
        subset(filex_name == .filex) |>
        with({
          filex_write <- filex |>
            unlist(recursive = FALSE)
          if('VBOSE' %in% colnames(filex_write$`SIMULATION CONTROLS`)){
            filex_write$`SIMULATION CONTROLS`$VBOSE <- 'N'
          }else if('LONG' %in% colnames(filex_write$`SIMULATION CONTROLS`)){
            filex_write$`SIMULATION CONTROLS`$LONG <- 'N'
          }
          DSSAT::write_filex(filex_write, .filex)
        })
    })

}
