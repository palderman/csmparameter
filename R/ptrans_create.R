# ptrans_arg_list <- function(expr){
#   arg_list <- all.vars(substitute(expr))
#   return(arg_list)
# }

ptrans_get_ind <- function(pnames, expr){
  ind <- match(all.vars(substitute(expr)), pnames)
  return(ind)
}

#' Create a parameter transformation function
#'
#' @export
#'
ptrans_create <- function(expr){
  arg_list <- all.vars(substitute(expr))
  new_f <- eval(parse(text = paste0("function(",
                                    paste(arg_list, collapse = ", ")
                                    ,
                                    "){\n",
                                    deparse(substitute(expr)),
                                    "\n}\n")))
  return(new_f)
}
