#' Use the Extended FAST method to perform global sensitivity analysis
#'
#' @export
#'
#' @param prm_sa a named list created using [prm_create_prm_sa]
#'
#' @param ... additional arguments passed on to the \code{fast99} function.
#'  See \link[sensitivity]{fast99}() for more details.
#'
prm_sa_efast <- function(prm_sa, ...){

  if(!suppressMessages(requireNamespace("sensitivity", quietly = TRUE))){
    stop("prm_sa_efast() requires the sensitivity package. Please install it and try again.")
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

  if("pmin" %in% colnames(true_prm_df)){
    lower <- true_prm_df$pmin
  }else{
    lower <- prm_get_pmin(true_prm_df$pdensity)
  }
  for(i in seq_along(true_prm_df[["psampler"]])){
    if(is.na(lower[i])){
      lower[i] <- true_prm_df[["psampler"]][[i]](n = 1, p = 0.01)
    }
  }
  stopifnot(!is.na(lower))

  if("pmax" %in% colnames(true_prm_df)){
    upper <- true_prm_df$pmax
  }else{
    upper <- prm_get_pmax(true_prm_df$pdensity)
  }
  for(i in seq_along(true_prm_df[["psampler"]])){
    if(is.na(upper[i])){
      upper[i] <- true_prm_df[["psampler"]][[i]](n = 1, p = 0.99)
    }
  }
  stopifnot(!is.na(upper))

  efast_args <- list(...)

  if(!"factors" %in% names(efast_args)){
    efast_args[["factors"]] <- true_prm_df[,"pname"]
  }

  if(!"n" %in% names(efast_args)){
    efast_args[["n"]] <- 1000
  }

  if(!"M" %in% names(efast_args)){
    efast_args[["M"]] <- 4
  }

  if(!"q" %in% names(efast_args)){
    efast_args[["q"]] <- "qunif"
  }

  if(!"q.arg" %in% names(efast_args)){
    efast_args[["q.arg"]] <- list(min = 0, max = 1)
  }

  efast_design <- do.call(sensitivity::fast99, args = efast_args)

  n_prm <- sum(true_prms)

  efast_df <- list()

  for(.c in names(prm_sa[["yhat_template"]])){
    efast_df[[.c]] <- prm_sa[["yhat_template"]][[.c]] |>
      rep(each = n_prm)
  }

  efast_stats <- run_design(efast_design,
                            compute_efast_statistics,
                            prm_sa)

  efast_df <-
    efast_df |>
    as.data.frame() |>
    cbind(efast_stats) |>
    as.data.frame()

  rownames(efast_df) <- NULL

  efast_df
}


compute_efast_statistics <- function(.efast){

  data.frame(
    prm = colnames(.efast$X),
    S1 = .efast$D1/.efast$V,
    St = 1 - .efast$Dt/.efast$V)

}
