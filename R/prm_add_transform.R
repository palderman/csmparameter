ptrans_internal <- function(arg_list, body){
  paste0("function(",
           paste(arg_list, collapse = ", "),
         "){",
            body,
         "}"
  )
}

prm_fill_in_pnum <- function(prm_df){
  fill_index <- is.na(prm_df$pnum)
  max_pnum <- max(prm_df$pnum, nrow(prm_df), na.rm = TRUE)
  pnum_left <- 1:max_pnum
  pnum_left <- pnum_left[!pnum_left %in% prm_df$pnum]
  prm_df$pnum[fill_index] <- pnum_left[1:sum(fill_index)]
  return(prm_df)
}

ptrans_find <- function(search, char_vec){
  index <- match(search, char_vec)
  index[index <= 0] <- NA
  return(index)
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

ptrans_get_pname <- function(expr){
  if(is.list(expr)){
    pname <- unlist(
      lapply(expr, function(.x){
        all.vars(
          getElement(.x, 2)
          )
      })
    )
  }else{
    pname <- all.vars(
      getElement(expr, 2)
    )
  }
  return(pname)
}

ptrans_get_arg_list <- function(expr){
  if(is.list(expr)){
    arg_list <- lapply(expr,
                       function(.x){
        all.vars(
          getElement(.x, 3)
        )
      })
  }else{
    arg_list <- all.vars(
      getElement(expr, 3)
    )
  }
  return(arg_list)
}

ptrans_get_body <- function(expr){
  if(is.list(expr)){
    body <- lapply(expr,
                   function(.x){
                     deparse(
                       getElement(.x, 3)
                       )
                     })
  }else{
    body <- deparse(
      getElement(expr, 3)
    )
  }
  return(body)
}

#' Create a parameter transformation
#'
#' @export
#'
prm_add_transform <- function(prm_df, ptrans, ...){

  if(!is.expression(ptrans)) ptrans <- as.expression(ptrans)

  prm_df$pnum <- 1:nrow(prm_df)

  fun_df <- lapply(as.list(ptrans), create_transform_function,
                   prm_df = prm_df, ...) |>
    Reduce(x = _,
           f = \(.x, .y) merge(.x, .y, all = TRUE))

  if(! "ptransform" %in% colnames(prm_df)){
    prm_df$ptransform <- vector(mode = "list", length = nrow(prm_df))
  }

  # Construct data frame of transformed parameters for
  #  subsequent merging with prm_df
  ptrans_df <-
    data.frame(pname = ptrans_get_pname(as.list(ptrans)),
               ...,
               stringsAsFactors = FALSE) |>
    prm_add_pregex() |>
    within({
      pdensity = rep(list(NULL), length(pname))
      psampler = rep(list(NULL), length(pname))
    }) |>
    merge(fun_df)

  # Add any transformed parameters to prm_df that are not
  #  already present
  prm_df <- merge(prm_df,
                  ptrans_df,
                  all = TRUE,
                  sort = FALSE)

  # Sort by pnum
  prm_df <- arrange_df(prm_df, "pnum")
  prm_df <- prm_fill_in_pnum(prm_df)

  # Find row index for transformed parameters in prm_df
  # ptrans_ind <- ptrans_find(ptrans_get_pname(as.list(ptrans)),
  #                           prm_df$pname)

  # Add pfile to corresponding rows
  # if("pfile" %in% names(prm_df)){
  #   prm_df$pfile[ptrans_ind] <- pfile
  # }

  # Add transform functions to corresponding rows
  # prm_df$ptransform[ptrans_ind] <- fun_list

  # nullify density functions for transformed parameters
  # prm_df$pdensity[ptrans_ind] <- rep(list(NULL), length(ptrans_ind))

  # nullify prior sample functions for transformed parameters
  # prm_df$psampler[ptrans_ind] <- rep(list(NULL), length(ptrans_ind))

  prm_df |>
    as_prm_df() |>
    standardize_column_order()
}

create_transform_function <- function(ptr, prm_df, ...){

  pname <- ptrans_get_pname(ptr)

  arg_list <- ptrans_get_arg_list(ptr)

  if(pname %in% arg_list){
    stop(
      paste0(pname,
             " found in the parameter transform expression: ",
             deparse(ptr),
             "\n Please create a corresponding latent variable (e.g. ",
             pname, "_latent).")
    )
  }

  .dots <- list(...)

  .dots_lengths <-
    .dots |>
    lapply(length) |>
    unlist()

  .dots <-
    .dots |>
    lapply(\(.x){
      if(length(.x) == 1){
        rep(.x, max(.dots_lengths))
      }else{
        .x
      }
    })

  .dots_lengths <-
    .dots |>
    lapply(length) |>
    unlist()

  if(length(.dots) > 0){
    stopifnot(all(.dots_lengths == max(.dots_lengths)))
    p_nrow <- max(.dots_lengths)
  }else{
    p_nrow = 1
  }

  ptrans_df <- c(
    list(pname = rep(arg_list, each = p_nrow)),
    list(arg_num = rep(1:length(arg_list), each = p_nrow)),
    lapply(.dots, rep, times = length(arg_list)),
    list(stringsAsFactors = FALSE)) |>
    do.call(data.frame, args = _)

  arg_df <-
    ptrans_df[["pname"]] |>
    unique() |>
    lapply(\(.pn){

      .cn <- colnames(prm_df)
      .cn <- .cn[.cn %in% c("pname", "pnum", colnames(ptrans_df))]

      .prm_df <- prm_df |>
        subset(pname == .pn, select = .cn) |>
        lapply(\(.x) if(any(!is.na(.x))) list(.x) else NULL) |>
        do.call(c, args = _) |>
        as.data.frame()

      .ptrans_df <-
        ptrans_df |>
        subset(pname == .pn)

      merge(.prm_df, .ptrans_df, all.y = TRUE)
    }) |>
    Reduce(x = _,
           f = \(.x, .y) merge(.x, .y, all = TRUE))

  colnames(arg_df) <-
    colnames(arg_df) |>
    gsub("^pname$", "arg_name", x = _)

  body <- ptrans_get_body(ptr)

  fun_df <-
    aggregate(arg_df[,c("arg_name", "pnum", "arg_num")],
              arg_df[names(.dots)],
              list) |>
    within({
      ptransform = mapply(
        .arg_name = arg_name,
        .pnum = pnum,
        .arg_num = arg_num,
        \(.arg_name, .pnum, .arg_num){
          ptrans_fun(.arg_name[order(.arg_num)],
                     body,
                     .pnum[order(.arg_num)])
        })
      arg_name = NULL
      pnum = NULL
      arg_num = NULL
    }) |>
    within({
      pname = pname
    })

  fun_df
}
