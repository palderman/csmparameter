#'
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect str_replace
#' @importFrom lubridate is.POSIXct
#' @importFrom dplyr  left_join mutate_if select
#'
#' @export
#'
dat_to_dap <- function(pdate,.data){

  dat_cols <- map_lgl(.data,~is.POSIXct(.)) & !str_detect(colnames(.data),'^DATE$')

  if(any(dat_cols)){
    dat_cnames <- colnames(.data)[dat_cols]
    .data <- .data |>
      left_join(pdate) |>
      (\(.x)
      mutate_if(.x,
                colnames(.x) %in% dat_cnames,
                ~{as.numeric(difftime(., PDATE,units='days'))})
      )() |>
      select(-PDATE)

    cnames <- colnames(.data)

    cnames[cnames %in% dat_cnames] <- cnames[cnames %in% dat_cnames] |>
      str_replace('T$','P')

    colnames(.data) <- cnames
  }

  return(.data)
}
