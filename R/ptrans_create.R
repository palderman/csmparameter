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
ptrans_create <- function(pnames, expr){
  arg_list <- all.vars(substitute(expr))
  body <- deparse(substitute(expr))
  pind <- match(arg_list, pnames)
  fun <- ptrans_fun(arg_list, body, pind)
  return(fun)
}
