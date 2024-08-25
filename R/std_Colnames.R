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
      if(Ref_num==length(col_Virus)){  ## nrow == ncol (in Ref)
        col_Virus <- 1:Ref_num
      }else if(all(nchar(col_Virus)!=0)){

      }else if(length(which(nchar(col_Virus)==0))==Ref_num){
        col_Virus[which(nchar(col_Virus)==0)] <- 1:Ref_num
      }
      colnames(Tb)[5:(ncol(Tb)-1)] <- col_Virus
      Tb
    })
  names(result) <- names(TB_list)
  return(result)
}
