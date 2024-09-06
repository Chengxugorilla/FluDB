add_Nicknames <- function(TB_list,type){
  result <-
    lapply(seq_along(TB_list), function(i){
      print(i)
      x <- TB_list[[i]]
      x$Nicknames <-
        unlist(
          WHO_standardization(x$Virus,type))
      x
    })

  names(result) <- names(TB_list)
  return(result)
}
