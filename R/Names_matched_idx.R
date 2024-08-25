#' @title Obtain index of GISAID
#' @description A short description...
#' @param V_names description
#' @param GISAID_ref description
#' @export

Names_matched_idx <- function(V_names,GISAID_ref){
  idx_Virus <- grep(V_names,GISAID_ref[,1],fixed = TRUE)
  nick_V_name <- stringr::str_extract_all(V_names, "A/[^/]+/[^/]+/\\d+")[[1]]

  if(!identical(nick_V_name,character(0)) & identical(idx_Virus,integer(0))){
    idx_Virus <- unlist(
      lapply(nick_V_name,function(x){
        grep(x,GISAID_ref[,1],fixed = TRUE)
      }))}

  if(!identical(nick_V_name,character(0)) & identical(idx_Virus,integer(0))){
    idx_Virus <- unlist(
      lapply(nick_V_name,function(x){
        grep(x,GISAID_ref[,5],fixed = TRUE)
      }))}

  return(idx_Virus)
}
