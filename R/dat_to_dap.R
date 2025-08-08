dat_to_dap <- function(pdate,.data){

  dat_cols <- sapply(.data, \(.x) "POSIXt" %in% class(.x)) &
    ! grepl("^DATE$", colnames(.data))

  if(any(dat_cols)){

    dat_cnames <- colnames(.data)[dat_cols]

    .data <-
      .data |>
      merge(pdate, all.x = TRUE)

    for(i in seq_along(.data)){
      if(colnames(.data)[i] %in% dat_cnames){
        .data[[i]] <-
          .data[[i]] |>
          difftime(PDATE, units='days') |>
          as.numeric()
      }
    }

    .data[["PDATE"]] <- NULL

    cnames <- colnames(.data)

    cnames[cnames %in% dat_cnames] <-
      cnames[cnames %in% dat_cnames] |>
      gsub('T$','P', x = _)

    colnames(.data) <- cnames
  }

  return(.data)
}
