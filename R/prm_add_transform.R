ptrans_internal <- function(arg_list, body){
  paste0("function(",
           paste(arg_list, collapse = ", "),
         "){",
            body,
         "}"
  )
}

ptrans_fun <- function(arg_list, body, pind){
  internal_fun <- ptrans_internal(arg_list, body)
  new_f <- eval(parse(text = paste0("function(prm){",
                                      "do.call(",
                                        internal_fun,
                                        ",",
                                        "as.list(prm[c(",
                                          paste(pind, collapse = ", "),
                                        ")])",
                                      ")",
                                    "}"
                                    )))
  return(new_f)
}

#' Create a parameter transformation
#'
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr right_join n
#'
prm_add_transform <- function(prm_tbl, ptrans, ...){
  pname <- all.vars(ptrans[[2]])
  arg_list <- all.vars(ptrans[[3]])
  ptrans_tbl <- tibble(pname = arg_list, ...)
  pind <- prm_tbl %>%
    ungroup() %>%
    mutate(pnum = 1:n()) %>%
    {suppressMessages(right_join(., ptrans_tbl))} %>%
    pull(pnum)
  body <- deparse(ptrans[[3]])
  fun <- ptrans_fun(arg_list, body, pind)
  new_tbl <- tibble(pname = pname, ptrans = list(fun)) %>%
    full_join(prm_tbl, ., by = pname)
  return(new_tbl)
}
