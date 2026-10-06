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

  if(!requireNamespace("sensitivity")){
    stop("prm_run_morris() requires the sensitivity package. Please install it and try again.")
  }

  true_prms <-
    prm_sa[["prm_df"]][["ptransform"]] |>
    lapply(is.null) |>
    unlist()

  true_prm_df <- prm_sa[["prm_df"]][true_prms,]

  if("pmin" %in% colnames(true_prm_df)){
    lower <- true_prm_df$pmin
  }else{
    lower <- prm_get_pmin(true_prm_df$pdensity)
  }
  lower <- lower[!is.na(lower)]

  if("pmax" %in% colnames(true_prm_df)){
    upper <- true_prm_df$pmax
  }else{
    upper <- prm_get_pmax(true_prm_df$pdensity)
  }
  upper <- upper[!is.na(upper)]

  morris_args <- list(...)

  morris_args[["binf"]] <- lower
  morris_args[["bsup"]] <- upper

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

  sim_mat <-
    morris_design$X |>
    apply(1, prm_sa[["sa_fun"]])

  return()

  # morris_design$X |>


  if(is.null(control$initialpop)){
    if(is.null(control$NP) | is.na(control$NP) | control$NP < 4){
      NP <- 10*length(lower)
    }else{
      NP <- control$NP
    }
    control$initialpop <- prm_sample_prior(prm_sa$prm_df, n = NP)
  }

  return(est_out)

}


compute_morris_statistics <- function(.morris, v_names){

  mu <- apply(.morris$ee, 3, \(M){
    apply(M, 2, mean)
  })

  mu_star <- apply(abs(.morris$ee), 3, \(M){
    apply(M, 2, mean)
  })

  sigma <- apply(.morris$ee, 3, \(M){
    apply(M, 2, sd)
  })

  if(missing(v_names)) v_names <- paste0("v", 1:ncol(mu))

    morris_stats <-
    list(mu = mu, mu_star = mu_star, sigma = sigma) |>
    lapply(\(.x) data.frame(variable = rep(v_names, each = nrow(.x)),
                            prm = rep(rownames(.x), length(v_names)),
                            value = as.vector(.x)))

  for(i in seq_along(morris_stats)){
    morris_stats[[i]][["stat"]] <- names(morris_stats)[i]
  }

  stats_df <-
    do.call(rbind, args = morris_stats) |>
    reshape(timevar = "stat",
            idvar = c("variable","prm"),
            direction = "wide")
  rownames(stats_df) <- NULL
  colnames(stats_df) <- colnames(stats_df) |>
    gsub("^value\\.", "", x = _)

  stats_df
}
