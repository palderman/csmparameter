parse_select <- function(.cnames, expr){

  # it is a call, must determine if negation present
  negation <- as.character(expr[[1]]) == "-"
  if(negation){
    if(is.name(expr[[2]])){
      # only negated name
      cnames_out <- cn[! cn == as.character(expr[[2]])]
    }else{
      # negated expression
      expr_char <- as.character(expr[[2]])
      if(expr_char[1] == "matches"){
        if(is.name(expr[[2]][[2]])){
          cnames_out <- eval.parent(expr[[2]][[2]], 2)
        }
      }
      # expand_arg <-
    }
  }else if(as.character(expr[[1]]) == "everything"){
    cnames_out <- .cnames
  }else if(as.character(expr[[1]]) == "matches"){
    cnames_out <- grep(as.character(expr[[2]]), .cnames, value = TRUE)
  }

  return(cnames_out)
}

# minimalist replacement for dplyr::select
select_new <- function(.data, ...){
  mc <- match.call(expand.dots = FALSE)
  cn <- colnames(.data)

  abcde <- letters[1:5]

  keep_list <- vector(mode = "list", length = length(mc$...))
  for(i in seq_along(mc$...)){
    arg <- mc$...[[i]]
    if(is.name(arg)){
      # it is just a name, convert to character
      keep_list[[i]] <- as.character(arg)
    }else if(is.call(arg)){
    }
  }
  keep <-
    do.call(c, keep_list) |>
    unique()
  .data[, keep, drop = FALSE]
}

# minimalist replacement for dplyr::rename
rename_new <- function(.data, ...){
  mc <- match.call(expand.dots = FALSE)

  new_names <- names(mc$...)
  orig_names <- as.character(mc$...)

  cn <- colnames(.data)
  for(i in seq_along(new_names)){
    cn[cn == orig_names[i]] <- new_names[i]
  }
  colnames(.data) <- cn

  return(.data)
}

# Minimalist replacements for dplyr::left_join, dplyr::right_join, etc.
join_new <- function(.x, .y, by = NULL, ...){
  if(is.null(by)){
    by.x <- by.y <- intersect(names(.x), names(.y))
  }else{
    by.x = names(by)
    by.y = as.vector(by)
    if(is.null(by.x)) by.x <- by.y
  }
  joined_df <- merge(.x, .y,
                     by.x = by.x,
                     by.y = by.y,
                     ...)
  return(joined_df)
}

left_join_new <- function(.x, .y, by = NULL){
  join(.x, .y, by = by, all.x = TRUE)
}

right_join_new <- function(.x, .y, by = NULL){
  join(.x, .y, by = by, all.y = TRUE)
}

full_join_new <- function(.x, .y, by = NULL){
  join(.x, .y, by = by, all = TRUE)
}

inner_join_new <- function(.x, .y, by = NULL){
  join(.x, .y, by = by, all = FALSE)
}
