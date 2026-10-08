#' Create a function for summarizing model outputs
#'
#' @export
#'
#' @param prm_name a character string providing the name of the parameter
#'  vector in the exressions
#'
#' @param time_name a character string providing the name of the time variable
#'  in the model output
#'
#' @param ... named expressions each of which can be evaluated with model
#'  outputs to generate summary output
#'
prm_create_smry_fun <- function(prm_name = "prm", time_name = "time", ...){

  .dots <- list(...)

  lapply(.dots, \(.d){
    if("formula" %in% class(.d)){
      as.character(.d)[-1]
    }else{
      as.character(.d)
    }}) |>
    mapply(.d = _,
           .n = names(.dots),
           SIMPLIFY = FALSE,
           FUN = \(.d, .n){
             paste0("smry_df[[\"", .n , "\"]] <- with(out_df, {", .d, "})")
           }) |>
    unlist() |>
    c(
      paste0("function(out_df, ", prm_name, "){"),
      "  smry_df <- list()",
      paste0("smry_df[[\"", time_name, "\"]] <- NA_real_"),
      "",
      x = _,
      "  as.data.frame(smry_df) |> ",
      "    merge(x = out_df, y = _, all = TRUE)",
      "}") |>
    parse(text = _) |>
    eval(envir = .GlobalEnv)
}
