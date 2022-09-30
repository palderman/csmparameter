#'
#' @importFrom stringi stri_rand_strings
#' @importFrom stringr str_detect
#'
generate_pregex <- function(widths){

  for(i in 1:1000){
    pregex <- stringi::stri_rand_strings(length(widths), widths)
    found = FALSE
    for(j in 1:length(pregex)){
      if(!found){
        found = any(str_detect(pregex[-j], pregex[j]))
      }else{
        break
      }
    }
    if(!found) break
  }

  return(pregex)

}
