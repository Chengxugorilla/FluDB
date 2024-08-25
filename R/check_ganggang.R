#' @title title
#' @description d
#' @param TB_list description
#' @export

check_ganggang <- function(TB_list){
  vnames <- unlist(
    lapply(TB_list,function(x){
      x$Virus
    }))

  idx_wrong <- which(sapply(vnames, function(s){
    stringr::str_count(s,"/") != 3
  }))
  if(length(idx_wrong)!=0)
    message(paste(vnames[idx_wrong],"\n"))
}
