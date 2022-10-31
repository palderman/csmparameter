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
#' @importFrom dplyr right_join n filter mutate ungroup full_join
#' @importFrom purrr map map_chr
#'
prm_add_transform <- function(prm_tbl, ptrans, ...){

  if(!is.expression(ptrans)) ptrans <- as.expression(ptrans)

  prm_tbl <- prm_tbl %>%
    ungroup() %>%
    mutate(pnum = 1:n())

  fun_list <- map(ptrans, function(ptr){
    pname <- ptr %>%
      getElement(2) %>%
      all.vars()

    arg_list <- ptr %>%
      getElement(3) %>%
      all.vars()

    if(pname %in% arg_list){
      str_c(pname,
            " found in the parameter transform expression: ",
            deparse(ptr),
            "\n Please create a corresponding latent variable (e.g. ",
            pname, "_latent).") %>%
        stop()
    }

    ptrans_tbl <- tibble(pname = arg_list, ...)

    pind <- prm_tbl %>%
      {suppressMessages(right_join(., ptrans_tbl))} %>%
      pull(pnum)

    # if(pname %in% arg_list){
    #   body <- ptr %>%
    #     getElement(3) %>%
    #     list(list(str_c(pname, "_latent"))) %>%
    #     setNames(pname) %>%
    #     do.call(substitute, .)
    # }else{
    body <- ptr %>%
      getElement(3) %>%
      deparse()
    # }

    fun <- ptrans_fun(arg_list, body, pind)

    return(fun)
  })

  if(! "ptrans" %in% colnames(prm_tbl)){
    prm_tbl <- prm_tbl %>%
      mutate(ptransform = vector("list", n()))
  }

  prm_tbl <- tibble(pname = map_chr(ptrans,~{
    .x %>%
      getElement(2) %>%
      all.vars()
      })) %>%
    full_join(prm_tbl, .) %>%
    arrange(pnum)

  ptrans_ind <- tibble(
      pname = map_chr(ptrans, ~{
        pname <- .x %>%
          getElement(2) %>%
          all.vars()
        }),
      pt_ind = 1:length(ptrans),
      ...) %>%
    full_join(prm_tbl, .) %>%
    filter(!is.na(pt_ind)) %>%
    pull(pnum)

  prm_tbl$ptransform[ptrans_ind] <- fun_list

  prm_tbl$pdensity[ptrans_ind] <- rep(list(NULL), length(ptrans_ind))

  prm_tbl$psampler[ptrans_ind] <- rep(list(NULL), length(ptrans_ind))

  return(prm_tbl)
}
