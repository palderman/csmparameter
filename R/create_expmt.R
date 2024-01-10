#' @export
#'
#' @importFrom DSSAT read_filea read_filex read_filet write_filex
#' @importFrom dplyr  filter mutate select
#' @importFrom stringr str_replace str_detect str_sub
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#'
create_expmt <- function(filex_name, trno=NULL, data_types=NULL,
                               rewrite_filex = FALSE){

  if(is.null(trno)){
    filex <- read_filex(filex_name)
    trno <- filex$TREATMENTS$N
  }

  filea_name <- filex_name |>
    str_replace('X$','A')
  if(file.exists(filea_name)){
    filea <- read_filea(filea_name)
    filea_col_names <- filea |>
      colnames() |>
      (\(.x) .x[! .x %in% c('TRNO','DATE')])()
    if(any(str_detect(colnames(filea),'DAT$'))){
      if(!exists('filex', envir = environment(fun = NULL))){
        filex <- read_filex(filex_name)
      }
      pdate <- get_pdate(filex, trno)
      filea <- dat_to_dap(pdate,filea)
    }else{
      pdate <- tibble(TRNO=numeric(),PDATE=numeric()) |>
        mutate(PDATE = as.POSIXct(PDATE,origin='1970-01-01',tz='UTC'))
    }
  }else{
    filea <- NULL
    filea_col_names <- NULL
  }

  filet_name <- filex_name |>
    str_replace('X$','T')
  if(file.exists(filet_name)){
    filet <- read_filet(filet_name)
    filet_col_names <- filet |>
      colnames() |>
      (\(.x) .x[! .x %in% c('TRNO','DATE')])()
  }else{
    filet <- NULL
    filet_col_names <- NULL
  }

  if(is.null(data_types)){
    data_types <- c(filea_col_names,filet_col_names) |>
      unique()
  }

  if(rewrite_filex){
    if(!exists('filex', envir = environment(fun = NULL))){
      filex <- read_filex(filex_name)
    }
    write_filex(filex,basename(filex_name))
  }else{
    file.copy(filex_name,basename(filex_name))
  }

  filex_name <- basename(filex_name)

  joined_data <- join_filea_filet(filea,filet) |>
    filter(TRNO %in% trno) |>
    pivot_longer(names_to='variable',values_to = 'obs',cols=c(-TRNO,-DATE)) |>
    mutate(EXPERIMENT=str_sub(filex_name,1,8)) |>
    select(EXPERIMENT,TRNO,DATE,everything()) |>
    filter(!is.na(obs) & variable %in% data_types)

  sim_data_template <- joined_data |>
    select(EXPERIMENT,TRNO,DATE,variable)

  if(!exists('pdate', envir = environment(fun = NULL))){
    pdate <- tibble(TRNO=numeric(),PDATE=numeric()) |>
      mutate(PDATE = as.POSIXct(PDATE,origin='1970-01-01',tz='UTC'))
  }

  sim_template <- tibble(data_template = list(sim_data_template),
                         pdate = list(pdate))

  if(!exists('filex', envir = environment(fun = NULL))){
    filex <- NULL
  }

  expmt <- tibble(filex_name = filex_name, filex = list(filex),
                  obs_tbl = list(joined_data), trno = list(trno),
                  data_types = list(data_types), sim_template = list(sim_template)) |>
    add_output_tbl() |>
    as_prm_expmt_tbl()

  return(expmt)

}
