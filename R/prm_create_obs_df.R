#' Create a table of observations for parameter estimation
#'
#' @export
#'
#' @param data_df a data frame of seasonal summary and/or within-season data to
#   use for parameter estimation in wide format. The data frame should contain
#'  a column for time (named \code{time} or another name as specified in the
#'  \code{time_alias} argument). Columns with grouping variables should be
#'  listed in the \code{group_cols} argument. The grouping variables should
#'  match those provided to \link{prm_create_sim} or \link{prm_create_sim_df}.
#'
#' @param time_alias an optional expression, providing a new name to use
#'  for the time variable. This name should match what the model returns.
#'
#' @param group_cols an expression, indicating columns in \code{data_df}
#'  containing grouping variables.
#'
#' @returns a data frame in long format that includes columns for time
#'  (named \code{time} or another name as specified in the \code{time_alias}
#'  argument), variable name (named \code{variable}) and observed data (named
#'  \code{obs}).
#'
prm_create_obs_df <- function(data_df, time_alias, group_cols){

  if(missing(time_alias)) time_alias <- "time"

  .cnames <-
    names(data_df) |>
    setNames(nm = _)

  if(missing(group_cols)){
    .gcol <- NULL
  }else{
    .gcol <-
      as.list(.cnames) |>
      eval(substitute(group_cols),
           envir = _,
           parent.frame())
  }

  .val_cols <- {.cnames[! names(.cnames) %in% c(time_alias, .gcol)]} |>
    unlist()

  data_df |>
    within({
      row_id = 1:nrow(data_df)
    }) |>
    reshape(times = .val_cols,
            timevar = "variable",
            v.names = "obs",
            varying = list(.val_cols),
            idvar = "row_id",
            direction = "long") |>
    within({
      row_id = NULL
    }) |>
    `rownames<-`(NULL) |>
    arrange_df(sort_vars = c(.gcol, time_alias))
}

arrange_df <- function(df, sort_vars){
  df[ do.call(order, args = df[sort_vars]), ]
}
