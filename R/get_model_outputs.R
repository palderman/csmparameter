#' @export
#'
#' @importFrom dplyr  is_grouped_df group_by select group_map
#' @importFrom tidyr crossing unnest
#'
get_model_outputs <- function(run_tbl){

  outputs <- run_tbl |>
    group_map(~{crossing(.y,.x) |>
        select(filex_trno) |>
        unnest(filex_trno) |>
        (\(.x) write_dssbatch(x = .x$filex_name,
                              trtno = .x$trno)
         )()
      dssat_out <- tryCatch({
        system(.x$dssat_call,intern = TRUE)
      },error = function(e){NULL})
      if(!is.null(dssat_out)){
        suppressMessages(read_sim_data(.x))
      }else{
        .x |>
          select(sim_template) |>
          unnest(sim_template) |>
          group_by(EXPERIMENT,TRNO) |>
          summarize() |>
          mutate(sim = NA_real_)
      }
    }) |>
    bind_rows()

  return(outputs)
}
