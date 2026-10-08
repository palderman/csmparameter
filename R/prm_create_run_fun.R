#' Create a function for running a model
#'
#' @export
#'
#' @param fun_name a character string providing the name of the function to be
#'  called for running each simulation
#'
#' @param prm_name a character string providing the name of the parameter
#'  vector
#'
#' @param time_name a character string providing the name of the time variable
#'  in the model output
#'
prm_create_run_fun <- function(fun_name,
                               prm_name = "prm",
                               time_name = "time"){
c("function(prm_vec, prm_est){",
  "  out_list <- ",
  "    prm_est[[\"sim_df\"]][[\"sim_args\"]] |> ",
  paste0("      lapply(\\(.a) c(list(", prm_name, " = prm_vec), .a)) |> "),
  paste0("      lapply(\\(.a) do.call(", fun_name, ", args = .a))"),
  "  if(!is.null(prm_est[[\"smry_fun\"]])){",
  "    out_list <- ",
  "      out_list |> ",
  paste0("      lapply(prm_est[[\"smry_fun\"]], ", prm_name, " = prm_vec)"),
  "  }",
  "  n_row_list <- lapply(out_list, nrow)",
  "  out_df <- ",
  "    sim_df |>",
  "    subset(select = -sim_args) |> ",
  "    as.list()",
  "  for(.c in names(out_df)){",
  "    out_df[[.c]] <- mapply(.col = out_df[[.c]],",
  "                           n_row = n_row_list,",
  "                           FUN = \\(.col, n_row) rep(.col, n_row),",
  "                           SIMPLIFY = FALSE)",
  "  }",
  "  yhat_filter_df <- ",
  "    prm_est[[\"yhat_template\"]] |> ",
  "    subset(select = -c(variable)) |> ",
  "    unique()",
  "  group_cols <- ",
  "    names(yhat_filter_df) |> ",
  paste0("    grep(\"",time_name,"\", x = _, invert = TRUE, value = TRUE)"),
  "  out_list |>",
  "    list() |> ",
  "    c(out_df) |>",
  "    c(FUN = list(cbind), SIMPLIFY = list(FALSE)) |>",
  "    do.call(mapply, args = _) |>",
  "    do.call(rbind, args = _) |>",
  "    merge(x = yhat_filter_df, y = _, all.x = TRUE, sort = FALSE) |>",
  paste0("    prm_create_obs_df(time_alias = \"", time_name, "\","),
  "                      group_cols = group_cols) |> ",
  "    within({",
  "      yhat = obs",
  "      obs = NULL",
  "    }) |> ",
  "    merge(x = prm_est[[\"yhat_template\"]],",
  "          y = _,",
  "          all.x = TRUE,",
  "          sort = FALSE)",
  "}") |>
    parse(text = _) |>
    eval(.GlobalEnv)
}
