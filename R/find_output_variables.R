find_output_variables <- function(.expmt,
                                  prioritize_files = c('Summary.OUT',
                                                       'PlantGro.OUT')){

  raw_out_files <- list.files(pattern = '(\\.OUT)|(\\.csv)') |>
    (\(.x) c(prioritize_files, .x))() |>
    unique() |>
    grep("Measured", x = _,
         value = TRUE, invert = TRUE) |>
    (\(.x) setNames(.x,.x))() |>
    map(function(.x) if(file.exists(.x)) readLines(.x) else NULL)

  data_type_regex <- .expmt$data_types |>
    unlist() |>
    paste0("(", .x = _, ")") |>
    paste0(collapse = "|")

  headers <- vector(mode = "list",
                    length = length(raw_out_files))
  for(i in seq_along(headers)){
    if(grepl("\\.csv$", names(raw_out_files)[i])){
      headers[[i]] <- raw_out_files[[i]][1]
    }else{
      headers[[i]] <- grep("^ *@", raw_out_files[[i]], value = TRUE)
    }
  }
  names(headers) <- names(raw_out_files)

  out_list <-
    headers |>
    sapply(\(.x) any(grepl(data_type_regex, .x))) |>
    subset(raw_out_files, subset = _) |>
    names() |>
    setNames(nm = _) |>
    lapply(\(.x) try(suppressWarnings(read_output(.x)), silent = TRUE))

  out_df <-
    out_list |>
    lapply(\(.x) ! "try-error" %in% class(.x) && any(colnames(.x) %in% unlist(.expmt$data_types))) |>
    subset(out_list, subset = _) |>
    (\(.x) data.frame(file_name = names(.x),
                      col_names = I(lapply(.x, colnames)),
                      data_types = I(list(unlist(.expmt$data_types))))
     )() |>
    within({
      dtype_check = mapply(intersect,
                           x = data_types,
                           y = col_names)
      nvars = dtype_check |>
        lapply(unlist) |>
        lapply(length) |>
        unlist()
    })

  out_df <-
    out_df[order(out_df[["nvars"]]), ]

  for(i in 1:nrow(out_df)){
    if(i < nrow(out_df)){
      out_df$col_names[i] <- setdiff(out_df$dtype_check[[i]],
                                      unlist(out_df$dtype_check[-1:-i])) |>
        list()
    }else{
      out_df$col_names[i] <- out_df$dtype_check[i]
    }
  }

  missing_data_types <- setdiff(unlist(.expmt$data_types),
                                unlist(out_df$col_names))

  if(length(missing_data_types) > 0){
    err_msg <-
      paste0(missing_data_types, collapse = ', ') |>
      paste0('The following data types were not found in any output file:\n',
             x = _,
             '\n Please check variable names or output options within File X.')
    warning(err_msg)
  }

  out_df <-
    out_df |>
    subset(sapply(out_df$col_names, \(.x) length(.x) > 0),
           select = c(file_name, col_names))

  return(out_df)

}
