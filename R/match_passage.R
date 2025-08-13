#' @title Obtain index of GISAID
#' @description A short description...
#' @param Passages description
#' @param GISAID_ref description
#' @export

match_passage <- function(Passages,GISAID_ref){
  ## if Passages is NA
  if(is.na(Passages)){
    idx_na <- which(is.na(GISAID_ref[,"Passage details/history"]))
    if(length(idx_na) == 0)
      idx_na <- 0
    return(idx_na)
  }

  ## fully matched
  idx_Passage <- grep(Passages,GISAID_ref[,"Passage details/history"],fixed = TRUE)

  ## CELL/EGG matched
  if(length(idx_Passage) == 0){
    idx_Passage <- which(unlist(
      lapply(seq_along(GISAID_ref[,"Passage details/history"]), function(i){
        passage_ident(Passages,GISAID_ref[i,"Passage details/history"])
      })))
  }

  return(idx_Passage)
}

passage_ident <- function(P1,P2){
  pi(P1) == pi(P2)
}
pi <- function(P){
  if(grepl("MDCK|SIAT|C|M|S",P)){
    return('CELL')
  }
  else if(grepl("E|cs",P)){
    return('EGG')
  }else if(grepl("Original",P)){
    return('Origin')
  }else{
    warning(P)
    return()
  }
}

# passage_attribute <- function(passage){
#   P_string <- strsplit(passage,split = '/')[[1]]
#   P_history <- list(MDCK=c(),SIAT=c(),C=c(),E=c(),D=c())
#
#   letters_part <- stringr::str_extract(P_string, "\\D+")
#   numbers_part <- as.numeric(stringr::str_extract(P_string, "\\d+"))
#   if(length(letters_part) != length(numbers_part))
#     return(P_history)
#
#   for (i in seq_along(letters_part)) {
#     p <- letters_part[i]
#     generation <- numbers_part[i]
#     if(p=='MDCK')
#       P_history[['MDCK']] <- c(P_history[['MDCK']],generation)
#     if(p=='SIAT')
#       P_history[['SIAT']] <- c(P_history[['SIAT']],generation)
#     if(p=='C')
#       P_history[['C']] <- c(P_history[['C']],generation)
#     if(p=='E')
#       P_history[['E']] <- c(P_history[['E']],generation)
#     if(p=='D')
#       P_history[['D']] <- c(P_history[['D']],generation)
#   }
#   return(P_history)
# }
#
# passage_distance <- function(passage_str){
#   P1 <- strsplit(passage_str[1],split = '/')[[1]]
#   P2 <- strsplit(passage_str[2],split = '/')[[1]]
#
# }
