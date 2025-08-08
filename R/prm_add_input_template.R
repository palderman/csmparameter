#' Add input template
#'
#' @export
#'
prm_add_input_template <- function(.input_df, .prm_df){

  .input_df <-
    .input_df |>
    by(~file_name,
       \(.x){
         pt <-
           .prm_df |>
           subset(pfile == .x$file_name) |>
           within({
             pfmt = gsub("(\\..*)|([a-z])", "s", pfmt)
           })
         if(nrow(pt) > 0){
           fp <-
             .x[["file_processed"]] |>
             unlist(recursive = FALSE)
           for(i in 1:nrow(pt)){
             pname <- pt[["pname"]][i]
             fp[[pname]] <- sprintf(pt$pfmt[i], fp[[pname]])
             if(grepl("\\.CUL", .x$file_name)){
               fp[[pname]][fp$`VAR#` == pt$pkey[i]] <- pt$pregex[i]
             }else if(grepl("\\.ECO", .x$file_name)){
               fp[[pname]][fp$`ECO#` == pt$pkey[i]] <- pt$pregex[i]
             }
             attr(fp,'v_fmt') <- modify_vfmt(fp, pt$pname[i], pt$pfmt[i])
           }
           .x$file_processed <- list(fp)
         }
         return(.x)
       }) |>
    do.call(rbind, args = _) |>
    within({
      file_template = generate_file_template(file_name,file_processed[[1]])
    }) |>
    subset(select = -file_processed)

  return(.input_df)
}
