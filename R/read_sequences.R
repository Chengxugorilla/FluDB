#' @title title
#' @description A short description...
#' @param file description

read_sequences <- function(file){
  sequences <- Biostrings::readDNAStringSet(file)
  return(list(names=names(sequences),sequences=as.vector(as.character(sequences))))
}
