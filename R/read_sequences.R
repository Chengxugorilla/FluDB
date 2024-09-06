#' @title title
#' @description A short description...
#' @param file description
#' @param seq_type description

read_sequences <- function(file,seq_type){
  sequences <- switch(seq_type,
                      DNA = Biostrings::readDNAStringSet(file),
                      AA = Biostrings::readAAStringSet(file))
  return(list(names=names(sequences),sequences=as.vector(as.character(sequences))))
}
