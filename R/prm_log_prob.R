#' @export
#'
prm_log_prob <- function(obs_df, sim_df, prm_df, pval){

  if(prm_check_sim(sim_df)){

    sigma_r_df <-
      prm_pval_df(pval, prm_df) |>
      prm_sigma_r_df()

    # Calculate prior density
    prior_lp <- prm_prior_log_density(pval, prm_df)

    log_likelihood <- obs_df |>
      # Combine observed and simulated data
      merge(sim_df, all.x = TRUE) |>
      merge(sigma_r_df, all.x = TRUE) |>
      # Calculate log-likelihood assuming errors normally distributed
      with(dnorm(obs, mean = sim, sd = pval, log = TRUE)) |>
      sum()

    lp <- prior_lp + log_likelihood

  }else{

    lp <- -.Machine$double.xmax

  }

  return(lp)

}

prm_sigma_r_df <- function(pval_df){

  sigma_r_df <-
    pval_df |>
    subset(grepl("^sigma_r;", pname))

  new_col_names <-
    sigma_r_df[["pname"]] |>
    gsub("^sigma_r;", "", x = _) |>
    gsub(":[^;]+", "", x = _) |>
    strsplit(";") |>
    unlist() |>
    unique()

  for(i in seq_along(new_col_names)){
    sigma_r_df[[new_col_names[i]]] <-
      paste0("(?<=", new_col_names[i], ":)[^;]+") |>
      gregexpr(pattern = _,
               text = sigma_r_df[["pname"]],
               perl = TRUE) |>
      regmatches(x = sigma_r_df[["pname"]],
                 m = _) |>
      unlist()
    if(new_col_names[i] == "DATE"){
      sigma_r_df[[new_col_names[i]]] <-
        as.POSIXct(sigma_r_df[[new_col_names[i]]])
    }else{
      check_type <- suppressWarnings(as.numeric(sigma_r_df[[new_col_names[i]]]))
      if(!any(is.na(check_type))){
        sigma_r_df[[new_col_names[i]]] <- check_type
      }
    }
  }

  sigma_r_df
}
