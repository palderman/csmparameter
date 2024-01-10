get_pdate <- function(filex, trno = NULL){

  if(is.null(trno)) trno <- unique(filex$TREATMENTS$N)

  pdate <- filex$TREATMENTS |>
    select(N, MP) |>
    rename(TRNO = N) |>
    left_join(filex$`PLANTING DETAILS`, by = c("MP"="P")) |>
    select(TRNO, PDATE) |>
    filter(TRNO %in% trno)

  return(pdate)

}
