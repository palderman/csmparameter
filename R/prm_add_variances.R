#' Add hyperparameters for variance terms ("model error") to prm_tbl
#'
#' @export
#'
#' @importFrom dplyr summarize mutate pull bind_rows bind_cols full_join
#'   group_by groups
#' @importFrom tidyr pivot_longer pivot_wider unite unnest
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @importFrom lubridate is.POSIXt
#'
#' @param obs_tbl a tibble as produced by the create_expmt()
#'  function grouped by the factors which identify the observation
#'  groups for which a variance term should be estimated
#'
prm_add_variances <- function(prm_tbl,
                              obs_tbl,
                              pmin = 0,
                              pmax = Inf,
                              pmu = 0,
                              psigma = 1,
                              pdist = "normal",
                              ...){

  prior_tbl <- tibble(pmin = pmin,
                    pmax = pmax,
                    pmu = pmu,
                    psigma = psigma,
                    pdist = pdist,
                    ...) |>
    # Join prior data with obs_tbl and suppress message
    (\(.x) suppressMessages(full_join(.x, obs_tbl))
     )() |>
    # Impose grouping structure from obs_tbl on var_tbl
    (\(.x) do.call(group_by, c(list(.x), groups(obs_tbl)))
     )() |>
    summarize(across(c(pmin, pmax, pmu, psigma, pdist), unique))

  var_tbl <- obs_tbl |>
    # Drop non-grouping variables
    summarize() |>
    ungroup() |>
    # Convert all grouping variables to character
    mutate(across(where(is.POSIXt), ~format(., "%Y-%j")),
           across(where(~{!is.character(.)}), as.character)) |>
    # Stack all grouping variables
    pivot_longer(everything()) |>
    # Append variable name to value
    mutate(value = str_c(name, value, sep = ":")) |>
    # Unstack appended grouping variables
    pivot_wider(values_fn = list) |>
    unnest(everything()) |>
    # Unite grouping variables into single pname column
    unite(pname, everything(), sep = ";") |>
    # Prepend pname with variance label
    mutate(pname = str_c("variance;", pname)) |>
    # Combine with tibble with prior information
    bind_cols(prior_tbl)

  output <- var_tbl |>
    # Create parameter table for variance parameters
    with(prm_create(pname = pname,
                    pfile = "",
                    pmin = pmin,
                    pmax = pmax,
                    pmu = pmu,
                    psigma = psigma,
                    pdist = pdist,
                    pnum = max(prm_tbl$pnum)+(1:length(pname)))) |>
    # Combine variance parameter table with original prm_tbl
    (\(.x) bind_rows(prm_tbl, .x)
     )()

  return(output)
}
