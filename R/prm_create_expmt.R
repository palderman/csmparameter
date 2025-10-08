#' Create an experiment for parameter estimation
#'
#' @export
#'
#' @param expmt either the file name of a DSSAT-formatted experiment
#'   file (File X) or a list as would be returned by
#'   \link[DSSAT]{read_filex} or \link[DSSAT]{filex_template}.
#'
#' @param data a data frame of seasonal and within-season data to use for
#'   parameter estimation or a character vector of names for DSSAT-formatted
#'   files for seasonal (File A) or within-season (File T) data
#'
prm_create_expmt <- function(expmt, data){

  if(is.null(trno)){
    filex <- DSSAT::read_filex(expmt)
    trno <- filex$TREATMENTS$N
  }

  filea_name <- gsub("X$", "A", filex_name)

  if(file.exists(filea_name)){
    filea <- DSSAT::read_filea(filea_name)
    filea_col_names <- filea |>
      colnames() |>
      grep("^(TRNO|DATE)$",
           x = _,
           invert = TRUE, value = TRUE)
    if(any(grepl("DAT$", colnames(filea)))){
      if(!exists('filex', envir = environment(fun = NULL))){
        filex <- DSSAT::read_filex(filex_name)
      }
      pdate <- get_pdate(filex, trno)
      filea <- dat_to_dap(pdate,filea)
    }else{
      pdate <- data.frame(TRNO=numeric(), PDATE=numeric()) |>
        within({
          PDATE = as.POSIXct(PDATE, origin='1970-01-01',tz='UTC')
        })
    }
  }else{
    filea <- NULL
    filea_col_names <- NULL
  }

  filet_name <- gsub("X$", "T", filex_name)

  if(file.exists(filet_name)){
    filet <- DSSAT::read_filet(filet_name)
    filet_col_names <- filet |>
      colnames() |>
      grep("^(TRNO|DATE)$",
           x = _,
           invert = TRUE, value = TRUE)
  }else{
    filet <- NULL
    filet_col_names <- NULL
  }

  if(is.null(data_types)){
    data_types <- c(filea_col_names, filet_col_names) |>
      unique()
  }

  # if(rewrite_filex){
  #   if(!exists('filex', envir = environment(fun = NULL))){
  #     filex <- read_filex(filex_name)
  #   }
  #   write_filex(filex,basename(filex_name))
  # }else{
  #   file.copy(filex_name,basename(filex_name))
  # }

  filex_name <- basename(filex_name)

  joined_data <- join_filea_filet(filea,filet) |>
    subset(TRNO %in% trno) |>
    pivot_longer(names_to = 'variable',
                 values_to = 'obs',
                 cols=c(-TRNO,-DATE)) |>
    within({
      EXPERIMENT=substr(filex_name, 1, 8)
      }) |>
    subset(!is.na(obs) & variable %in% data_types,
           select = c("EXPERIMENT", "TRNO", "DATE", everything()))

  sim_data_template <- joined_data |>
    subset(select = c("EXPERIMENT", "TRNO", "DATE", "variable"))

  if(!exists('pdate', envir = environment(fun = NULL))){
    pdate <- data.frame(TRNO=numeric(), PDATE=numeric()) |>
      within({
        PDATE = as.POSIXct(PDATE, origin='1970-01-01', tz='UTC')
      })
  }

  sim_template <- data.frame(data_template = I(list(sim_data_template)),
                             pdate = I(list(pdate)))

  if(!exists('filex', envir = environment(fun = NULL))){
    filex <- NULL
  }

  expmt <- data.frame(filex_name = filex_name,
                      filex = I(list(filex)),
                      obs_df = I(list(joined_data)),
                      trno = I(list(trno)),
                      data_types = I(list(data_types)),
                      sim_template = I(list(sim_template))) |>
    add_output_df() |>
    as_prm_expmt_df()

  return(expmt)

}
