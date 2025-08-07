#' @export
#'
prm_read_sim <- function(run_df){

  sim_template <-
    run_df[["sim_template"]] |>
    Reduce(f = rbind,
           x = _)

  if(nrow(sim_template) > 0){

    run_expmt <-
      sim_template |>
      subset(select = c("EXPERIMENT", "TRNO")) |>
      unique() |>
      within({
        RUN = seq_along(EXPERIMENT)
        RUNNO = RUN
      })

    out_df <-
      run_df[["out_df"]] |>
      Reduce(f = rbind,
             x = _)

    all_cols <-
      with(out_df,
           col_names)

    # Reject if any output file is missing
    # This assumes that if any output file is missing the simulation failed
    if(all(file.exists(out_df$file_name))){

      out <-
        out_df[["file_name"]] |>
        unique() |>
        lapply(\(.fn) read_model_output(.fn,
                                        sim_template,
                                        run_expmt,
                                        all_cols)) |>
        do.call(rbind, args = _) |>
        merge(sim_template, all.x = TRUE) |>
        within({
          sim = ifelse(grepl("DAT$", variable),
                       as.POSIXct(sim, tz='UTC', origin='1970-01-01') |>
                         difftime(PDATE, units="days") |>
                         as.numeric(),
                       sim)
        })

      }else{

        out <- sim_template

        out[["sim"]] <- NA_real_

      }
  }else{

    out <- sim_template

    out[["sim"]] <- NA_real_

  }

  out[["PDATE"]] <- NULL

  return(out)

}

read_model_output <- function(file_name, sim_template, run_expmt, all_cols){

  raw_output <- read_output(file_name)

  if(! "DATE" %in% names(raw_output)){
    raw_output[["DATE"]] <- as.POSIXct('0001001',format='%Y%j',tz='UTC')
  }

  merged_output <-
    raw_output |>
    subset(TRNO %in% sim_template$TRNO &
           DATE %in% sim_template$DATE) |>
    df_rename(RUN = "RUNNO") |>
    select(-matches("(EXPERIMENT)|(MODEL)")) |>
    merge(run_expmt, all = TRUE)

  select_cols <-
    all_cols |>
    c("EXPERIMENT", "TRNO", "DATE") |>
    paste0(collapse = "|") |>
    paste0("(", x = _, ")") |>
    grep(x = colnames(merged_output),
         values = TRUE)

  stack_cols <-
    select_cols |>
    grep(pattern = "(EXPERIMENT|TRNO|DATE)",
         x = _,
         values = TRUE,
         invert = TRUE)

  merged_output |>
    subset(select = select_cols) |>
    stack(select = stack_cols) |>
    df_rename(sim = "values",
              variable = "ind")

}
