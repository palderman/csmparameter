df_rename <- function(df, ...){

  rename_list <- list(...)

  new_names <- colnames(df)

  for(i in seq_along(rename_list)){
    new_names <- gsub(paste0("^", rename_list[[i]],"$"),
                      names(rename_list)[i],
                      new_names)
  }

  setNames(df, nm = new_names)
}
