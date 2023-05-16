rand_strings <- function(n, widths){

  alnum <- c(0:9, letters, LETTERS)

  strings_out <- vector(mode = "character", length = n)

  if(length(widths) == 1 & n > 1) widths <- rep(widths, n)

  for(i in 1:n){
    strings_out[i] <- paste0(sample(alnum, size = widths[i], replace = TRUE),
                             collapse = "")
  }

  return(strings_out)
}

generate_pregex <- function(widths){

  pregex <- rep(NA_character_, length = length(widths))

  for(i in 1:1000){

    pregex[!is.na(widths)] <- rand_strings(length(widths[!is.na(widths)]),
                                           widths[!is.na(widths)])
    found = FALSE
    for(j in 1:length(pregex)){
      if(!found){
        found = any(grepl(pregex[j], pregex[-j]))
      }else{
        break
      }
    }
    if(!found) break
  }

  return(pregex)

}
