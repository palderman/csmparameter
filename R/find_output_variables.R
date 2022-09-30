#' @importFrom dplyr "%>%" rowwise mutate ungroup arrange filter select
#' @importFrom tibble tibble
#' @importFrom purrr map map_lgl
#' @importFrom stringr str_c str_subset
#'
find_output_variables <- function(.expmt){

  raw_out_files <- list.files(pattern = '\\.OUT') %>%
    {c('Summary.OUT','PlantGro.OUT',.)} %>%
    unique() %>%
    str_subset("Measured", negate = TRUE) %>%
    setNames(.,.) %>%
    map(~readLines(.))

  data_type_regex <- .expmt$data_types %>%
    unlist() %>%
    str_c("(", ., ")") %>%
    str_c(collapse = "|")

  out_tbl <- raw_out_files %>%
    map_lgl(~any(str_detect(., "^ *@") &
                   str_detect(., data_type_regex))) %>%
    subset(raw_out_files, .) %>%
    names() %>%
    setNames(.,.) %>%
    map(~try(suppressWarnings(read_output(.)), silent = TRUE)) %>%
    {.[map_lgl(.,~{ ! 'try-error' %in% class(.) })]} %>%
    {.[map_lgl(.,~{ any(colnames(.) %in% unlist(.expmt$data_types)) })]} %>%
    {tibble(file_name = names(.), col_names = map(.,colnames),
            data_types = list(unlist(.expmt$data_types)))} %>%
    rowwise() %>%
    mutate(dtype_check = list(intersect(data_types,col_names)),
           nvars = length(unlist(dtype_check))) %>%
    ungroup() %>%
    arrange(nvars)

  for(i in 1:nrow(out_tbl)){
    if(i < nrow(out_tbl)){
      out_tbl$col_names[i] <- setdiff(out_tbl$dtype_check[[i]],
                                      unlist(out_tbl$dtype_check[-1:-i])) %>%
        list()
    }else{
      out_tbl$col_names[i] <- out_tbl$dtype_check[i]
    }
  }

  missing_data_types <- setdiff(unlist(.expmt$data_types),unlist(out_tbl$col_names))

  if(length(missing_data_types) > 0){
    err_msg <- str_c(missing_data_types,collapse = ', ') %>%
      str_c('The following data types were not found in any output file:\n',
            .,
            '\n Please check variable names or output options within File X.')
    warning(err_msg)
  }

  out_tbl <- filter(out_tbl,map_lgl(col_names,~{length(.) > 0})) %>%
    select(file_name,col_names)

  return(out_tbl)

}
