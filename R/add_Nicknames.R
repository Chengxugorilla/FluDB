add_Nicknames <- function(TB_list,type){
  result <-
    lapply(TB_list, function(x){
      x$Nicknames <-
        unlist(
          WHO_standardization(x$Virus,type))
      x
    })

  names(result) <- names(TB_list)
  return(result)
}
