#' @title title
#' @description d
#' @param TB_list description
#' @export

std_Colnames <- function(TB_list){
  result <-
    lapply(seq_along(TB_list), function(i){
      Tb <- TB_list[[i]]
      Ref_num <- length(which(Tb$Type=="Reference"))
      col_Virus <- colnames(Tb)[5:(ncol(Tb)-1)]
      col_absent_idx <- which(nchar(col_Virus)==0)
      
      if(length(col_absent_idx) == Ref_num){
        col_Virus[col_absent_idx] <- 1:Ref_num
      }
      
      colnames(Tb)[5:(ncol(Tb)-1)] <- col_Virus
      Tb
    })
  names(result) <- names(TB_list)
  return(result)
}
