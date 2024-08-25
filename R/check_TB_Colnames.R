#' @title title
#' @description d
#' @param TB_list description
#' @export

check_TB_Colnames <- function(TB_list){
  error_list <- list()
  TB_Names <- names(TB_list)
  for(i in seq_along(TB_list)){
    tb <- TB_list[[i]]
    checked_col <- check_columns(colnames(tb))
    if(!all(checked_col)){
      error_list[[TB_Names[i]]] <- which(!checked_col)
    }
  }
  error_list
}


#' @title title
#' @description d
#' @param dates description
#' @export

check_columns <- function(cnames) {
  cnames <- cnames[-c(1:4,length(cnames))]
  result <- stringr::str_detect(cnames, "^\\d+$")
  return(result)
}
