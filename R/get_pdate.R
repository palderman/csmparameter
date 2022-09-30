
#'
#' @importFrom dplyr "%>%" select rename left_join
#'
#' @export
#'
get_pdate <- function(filex){

  pdate <- filex$TREATMENTS %>%
    select(N, MP) %>%
    rename(TRNO = N) %>%
    left_join(filex$`PLANTING DETAILS`, by = c("MP"="P")) %>%
    select(TRNO, PDATE)

  return(pdate)

}
