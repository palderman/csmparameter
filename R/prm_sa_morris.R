#' Use the Morris method to perform global sensitivity analysis
#'
#' @export
#'
#' @param prm_sa a named list created using [prm_create_prm_sa]
#'
#' @param ... additional arguments passed on to the \code{morris} function.
#'  See \link[sensitivity]{morris}() for more details.
#'
prm_sa_morris <- function(prm_sa, ...){

  if(!suppressMessages(requireNamespace("sensitivity", quietly = TRUE))){
    stop("prm_sa_morris() requires the sensitivity package. Please install it and try again.")
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

  morris_args <- list(...)

  if(!"factors" %in% names(morris_args)){
    morris_args[["factors"]] <- true_prm_df[,"pname"]
  }

  if(!"r" %in% names(morris_args)){
    morris_args[["r"]] <- 4
  }

  if(!"design" %in% names(morris_args)){
    morris_args[["design"]] <- list(type = "oat", levels = 4, grid.jump = 2)
  }

  morris_design <- do.call(sensitivity::morris, args = morris_args)

  n_prm <- sum(true_prms)

  morris_df <- list()

  for(.c in names(prm_sa[["yhat_template"]])){
    morris_df[[.c]] <- prm_sa[["yhat_template"]][[.c]] |>
      rep(each = n_prm)
  }

  morris_stats <- run_design(morris_design,
                             compute_morris_statistics,
                             prm_sa)

  morris_df <-
    morris_df |>
    as.data.frame() |>
    cbind(morris_stats) |>
    as.data.frame()

  rownames(morris_df) <- NULL

  morris_df
}

run_design <- function(design, stat_fun, prm_sa){
    prm_sample_prior(prm_df,
                     n = ncol(design$X),
                     p = design$X) |>
    apply(1,
          prm_sa[["run_fun"]],
          prm_est = prm_sa,
          simplify = FALSE) |>
    lapply(\(.x) with(.x, yhat)) |>
    do.call(rbind, args = _) |>
    as.matrix() |>
    apply(2,
          \(.y) sensitivity::tell(design, y = .y),
          simplify =  FALSE) |>
    lapply(stat_fun) |>
    do.call(rbind, args = _) |>
    as.data.frame()
}



compute_morris_statistics <- function(.morris){

  mu <- apply(.morris$ee, 2, mean)

  mu_star <- .morris$ee |>
    abs() |>
    apply(2, mean)

  sigma <- apply(.morris$ee, 2, sd)

  stats_df <-
    data.frame(prm = colnames(.morris$ee),
               mu = mu,
               mu_star = mu_star,
               sigma = sigma)

  rownames(stats_df) <- NULL

  stats_df
}
