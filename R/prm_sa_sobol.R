#' Use the Sobol' method to perform global sensitivity analysis
#'
#' @export
#'
#' @param prm_sa a named list created using [prm_create_prm_sa]
#'
#' @param n the number of samples to generate
#'
#' @param ... additional arguments passed on to the \code{soboljansen} function.
#'  See \link[sensitivity]{soboljansen}() for more details.
#'
prm_sa_sobol <- function(prm_sa, n = 100, ...){

  if(!suppressMessages(requireNamespace("sensitivity", quietly = TRUE))){
    stop("prm_sa_sobol() requires the sensitivity package. Please install it and try again.")
  }

  if(is.null(prm_sa[["prm_df"]][["ptransform"]])){
    true_prms <- rep(TRUE, nrow = prm_sa[["prm_df"]])
  }else{
    true_prms <-
      prm_sa[["prm_df"]][["ptransform"]] |>
      lapply(is.null) |>
      unlist()
  }

  true_prm_df <- prm_sa[["prm_df"]][true_prms,]

  sobol_args <- list(...)

  sobol_args[["X1"]] <-
    runif(n*nrow(true_prm_df)) |>
    matrix(nrow = n)

  colnames(sobol_args[["X1"]]) <- true_prm_df[["pname"]]

  sobol_args[["X2"]] <-
    runif(n*nrow(true_prm_df)) |>
    matrix(nrow = n)

  colnames(sobol_args[["X2"]]) <- true_prm_df[["pname"]]

  sobol_design <- do.call(sensitivity::soboljansen, args = sobol_args)

  n_prm <- sum(true_prms)

  sobol_df <- list()

  for(.c in names(prm_sa[["yhat_template"]])){
    sobol_df[[.c]] <- prm_sa[["yhat_template"]][[.c]] |>
      rep(each = n_prm)
  }

  sobol_stats <- run_design(sobol_design,
                            compute_sobol_statistics,
                            prm_sa)

  sobol_df <-
    sobol_df |>
    as.data.frame() |>
    cbind(sobol_stats) |>
    as.data.frame()

  rownames(sobol_df) <- NULL

  sobol_df
}

compute_sobol_statistics <- function(.sobol){

  sobol_df <-
    .sobol$S |>
    as.data.frame()

  sobol_df[["prm"]] <- rownames(sobol_df)

  colnames(sobol_df) <-
    colnames(sobol_df) |>
    gsub("original", "S1", x = _)

  sobol_T <-
    .sobol$T |>
    as.data.frame()

  sobol_T[["prm"]] <- rownames(sobol_T)

  colnames(sobol_T) <-
    colnames(sobol_T) |>
    gsub("original", "St", x = _)

  sobol_df <-
    sobol_df |>
    subset(select = c(prm, S1)) |>
    merge(sobol_T, sort = FALSE) |>
    subset(select = c(prm, S1, St))

  rownames(sobol_df) <- NULL

  sobol_df
}
