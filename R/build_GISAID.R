#' @title Build GISAID reference information
#' @description return a list contain meta information of virus from a fasta file.
#' @param file the address of the fasta file.
#'

build_GISAID <- function(file,type){
  GISAID <- read_sequences(file)
  tt <- strsplit(GISAID[[1]],"|",fixed = TRUE)

  n.r <- unlist(
  lapply(tt, function(x){
    length(unlist(x)) == 5
  }))
  if(!all(n.r))
    stop(which(!n.r))
  result <- do.call("rbind",tt)
  colnames(result) <- c("Virus","Passage","Collection_Date","Clade","Isolate_ID")
  result <- as.data.frame(result)
  result$Virus <- sub(">","",result$Virus)
  result$FluDB_ID <- GISAID_standardization(result[,1],type=type)
  list(meta=result,sequences=GISAID[[2]])
}
