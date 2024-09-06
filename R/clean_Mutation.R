#' @title title
#' @description d
#' @param TB d

clean_Mutation <- function(TB){
  lapply(TB,function(x){
    x$Virus <- stringr::str_extract(x$Virus,"(A|B)/.*/([0-9]{4}|[0-9]{2})")
    x
  })
}
