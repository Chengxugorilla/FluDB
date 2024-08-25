#' @title title
#' @description A short description...
#' @param cases description
#' @param data description

absent_case <- function(cases,data){
  unlist(
    lapply(seq_along(cases),function(i){
    if(any(grepl(cases[i],data,fixed = TRUE))){
      return()
    }else{
      return(i)
    }
  }))
}
