#' Create an objective function
#'
#' @export
#'
#' @param obj an expression or formula calculating the value of the objective
#'  function in which the observed data are named \code{obs} and the
#'  corresponding simulated output data are named \code{yhat}
#'
#' @param aggr an expression or formula object for aggregating objective
#'  function values into a single scalar result in which objective function
#'  values are named \code{obj_val}
#'
#' @param by an optional formula providing a set of grouping factors by which to
#'  group the initial calculation of the objective function value. For more
#'  details, see the \code{f} argument of the \link{split} method for data
#'  frames.
#'
prm_create_obj_fun <- function(obj, aggr, by){

  obj_char <- obj |>
    as.character()

  if("formula" %in% class(obj)){
    obj_char <- obj_char[-1]
  }

  if(missing(by)){
    obj_calc <- c(
      "      within({",
      paste0("        obj_val = ", obj_char),
      "      }) |>")
  }else{
    obj_calc <- by |>
      as.character() |>
      getElement(-1) |>
      paste0("         INDICES = ~", x =  _, ",") |>
      c(paste0("      by(FUN = \\(.x) with(.x, ", obj_char, "), "),
        x = _,
        "         simplify = FALSE) |>",
        "      array2DF(responseName = 'obj_val') |> ")
  }

  aggr_char <- aggr |>
    as.character()

  if("formula" %in% class(aggr)){
    aggr_char <- aggr_char[-1]
  }

  aggr_calc <- paste0("    with(", aggr_char, ")")

  c("function (obs_df, yhat_df, ...) ",
    "{",
    "  if(prm_check_yhat(yhat_df)){",
    "    obj_val <- obs_df |>",
    "      merge(yhat_df, all = TRUE) |>",
    obj_calc,
    "      subset(!is.na(obj_val)) |>",
    aggr_calc,
    "  }else{",
    "    obj_val <- .Machine$double.xmax",
    "  }",
    "",
    "  obj_val",
    "}") |>
    parse(text = _) |>
    eval()
}
