#' Create an objective function
#'
#' @export
#'
#' @param expr an expression or formula calculating the value of the objective
#'  function in which the observed data are named \code{obs} and the
#'  corresponding simulated output data are named \code{yhat}
#'
#' @param aggr_fun a function for aggregating group-specific
#'  objective function values for groups defined by indicated in the \code{by} argument
#'  into a single scalar result. The expression should use the name
#'  \code{obj_val} for the group-specific objective function values
#'
#'  @param ... optional additional grouping factors for defining groups for the
#'  expressions in \code{expr}
#'
prm_create_obj_fun <- function(expr, aggr_fun, ...){

  if("formula" %in% class(expr)){
    expr <- expr |>
      as.character() |>
      tail(-1) |>
      parse(text = _)
  }

  .dots <- list(...)

  if(length(.dots) > 0){
    n_rows <- lapply(.dots,
                     \(.d) length(.d)) |>
      unlist() |>
      max()
    if(length(expr) == 1 & n_rows > 1){
      expr <- rep(expr, n_rows)
    }
    obj_df <- data.frame(
      expr_col = I(expr),
      ...
    )
  }else{
    obj_df <- data.frame(
      expr_col = I(expr)
    )
  }

  function(yhat_df, prm_est, prm_val){
      merge_cols <-
        intersect(names(prm_est[["obs_df"]]), names(yhat_df)) |>
        intersect(names(obj_df))
      if(length(merge_cols) > 0){
        aggr_by <-
          merge_cols |>
          paste0(collapse = "+") |>
          paste0("cbind(obs, yhat) ~ ", x = _) |>
          as.formula()
      }else{
        aggr_by <- as.formula("cbind(obs, yhat) ~ 1")
      }
      merge(prm_est[["obs_df"]], yhat_df, all.x = TRUE, sort = FALSE) |>
        aggregate(x = _, by = aggr_by, FUN = list) |>
        merge(obj_df, obs_yhat_df, all.x = TRUE, sort = FALSE) |>
        with(mapply(.e = expr_col,
                    .obs = obs,
                    .yhat = yhat,
                    .pval = pval,
                    SIMPLIFY = FALSE,
                    \(.e, .obs, .yhat, .pval){
                      with(c(list(obs = .obs, yhat = .yhat), .pval),
                                            eval(.e))})) |>
        unlist() |>
        aggr_fun()
  }

  # if(missing(by)){
  #   if(missing(aggr)){
  #     obj_calc <- c(
  #       "      with({",
  #       paste0("        ", obj_char),
  #       "      })")
  #   }else{
  #     obj_calc <- c(
  #       "      within({",
  #       paste0("        obj_val = ", obj_char),
  #       "      })")
  #   }
  # }else{
  #   obj_calc <- by |>
  #     as.character() |>
  #     getElement(-1) |>
  #     paste0("         INDICES = ~", x =  _, ",") |>
  #     c(paste0("      by(FUN = \\(.x) with(.x, ", obj_char, "), "),
  #       x = _,
  #       "         simplify = FALSE) |>",
  #       "      array2DF(responseName = 'obj_val')")
  # }
  #
  # if(!missing(aggr)){
  #
  #   aggr_char <- aggr |>
  #     as.character()
  #
  #   if("formula" %in% class(aggr)){
  #     aggr_char <- aggr_char[-1]
  #   }
  #
  #   aggr_calc <- paste0("    with(", aggr_char, ")")
  # }else{
  #   aggr_calc <- NULL
  # }
  #
  # c("function (obs_df, yhat_df, ...) ",
  #   "{",
  #   "  if(prm_check_yhat(yhat_df)){",
  #   "    obj_val <- obs_df |>",
  #   "      merge(yhat_df, all = TRUE, sort = FALSE) |>",
  #   obj_calc,
  #   # "      subset(!is.na(obj_val)) |>",
  #   aggr_calc,
  #   "  }else{",
  #   "    obj_val <- .Machine$double.xmax",
  #   "  }",
  #   "",
  #   "  obj_val",
  #   "}") |>
  #   parse(text = _) |>
  #   eval()
}
