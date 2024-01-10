#' @export
#'
#' @importFrom dplyr  filter group_by group_modify mutate ungroup select
#' @importFrom stringr str_replace_all str_detect
#' @importFrom DSSAT mutate_cond
#'
add_input_template <- function(.input_tbl,.prm_tbl){

  .input_tbl <- .input_tbl |>
    group_by(file_name) |>
    group_modify(~{
      pt <- filter(.prm_tbl, pfile == .y$file_name) |>
        mutate(pfmt = str_replace_all(pfmt,'(\\..*)|([a-z])','s'))
      if(nrow(pt) > 0){
        fp <- .x$file_processed[[1]]
        for(i in 1:nrow(pt)){
          fp <- mutate(fp,!!pt$pname[i] := sprintf(pt$pfmt[i],!!as.name(pt$pname[i])))
          if(str_detect(.y$file_name,'\\.CUL')){
            fp <- mutate_cond(fp,`VAR#` == pt$pkey[i], !!pt$pname[i] := !!pt$pregex[i])
          }else if(str_detect(.y$file_name,'\\.ECO')){
            fp <- mutate_cond(fp,`ECO#` == pt$pkey[i], !!pt$pname[i] := !!pt$pregex[i])
          }
          attr(fp,'v_fmt') <- modify_vfmt(fp,pt$pname[i],pt$pfmt[i])
        }
        .x$file_processed[[1]] <- fp
      }
      return(.x)
    }) |>
    mutate(file_template = generate_file_template(file_name,file_processed[[1]])) |>
    ungroup() |>
    select(-file_processed)

  return(.input_tbl)
}
