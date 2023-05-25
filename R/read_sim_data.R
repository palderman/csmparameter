#' @export
#'
#' @importFrom dplyr "%>%" group_by mutate filter full_join left_join group_map
#'   n ungroup select pull rename_with bind_rows
#' @importFrom purrr reduce
#' @importFrom tibble add_column
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer unnest
#'
read_sim_data <- function(run_tbl){

  run_expmt <- run_tbl %>%
    select(sim_template) %>%
    unnest(sim_template) %>%
    group_by(EXPERIMENT,TRNO) %>%
    summarize() %>%
    ungroup() %>%
    mutate(RUN = 1:n(),
           RUNNO = RUN)

  all_cols <- run_tbl %>%
    ungroup() %>%
    select(out_tbl) %>%
    unnest(out_tbl) %>%
    pull(col_names)

  out_tbl <- run_tbl %>%
    ungroup() %>%
    select(out_tbl) %>%
    unnest(out_tbl)

  # Reject if any output file is missing
  # This assumes that if any output file is missing the simulation failed
  if(all(file.exists(out_tbl$file_name))){
    out <- out_tbl %>%
      group_by(file_name) %>%
      group_map(~{
        # read_output(.y$file_name,read_only = c('TRNO','DATE','RUN','RUNNO',.x$col_names)) %>%
        read_output(.y$file_name) %>%
          {
            if( ! 'DATE' %in% names(.)){
              . <- add_column(.,DATE = as.POSIXct('0001001',format='%Y%j',tz='UTC'))
            }
            .
          } %>%
          filter(TRNO %in% run_tbl$sim_template[[1]]$TRNO &
                 DATE %in% run_tbl$sim_template[[1]]$DATE) %>%
          rename_with(~str_replace(.,'RUNNO','RUN')) %>%
          select(-matches("(EXPERIMENT)|(MODEL)")) %>%
          full_join(run_expmt) %>%
          select(any_of(c("EXPERIMENT", "TRNO", "DATE", all_cols))) %>%
          pivot_longer(names_to = "variable",
                       values_to = "sim",
                       cols = any_of(all_cols))
      }) %>%
      bind_rows() %>%
      left_join(run_tbl$sim_template[[1]],.) %>%
      mutate(sim = ifelse(str_detect(variable,'DAT$'),
                          as.numeric(difftime(as.POSIXct(sim,tz='UTC',origin='1970-01-01'),
                                              PDATE,
                                              units="days")),
                          sim)) %>%
      select(-PDATE)
    }else{
      out <- run_tbl$sim_template[[1]] %>%
        mutate(sim = NA_real_) %>%
        select(-PDATE)
    }

  return(out)

}
