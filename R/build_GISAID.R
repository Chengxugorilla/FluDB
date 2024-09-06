#' @title Build GISAID reference information
#' @description return a list contain meta information of virus from a fasta file.
#' @param file the address of the fasta file.
#' @param seq_type DNA or AA.
#'

build_GISAID <- function(file,type,seq_type){
  ## read sequence from file
  GISAID <- read_sequences(file,seq_type)
  Vinfo_list <- strsplit(GISAID[[1]],"|",fixed = TRUE)

  Vinfo_std_list <- 
    lapply(seq_along(Vinfo_list), function(i){
      temp <- Vinfo_list[[i]]
      if(length(temp) == 15){
        temp <- c(temp,NA)
      }else if(length(temp) != 16){
        warning(temp[1])
      }
      return(temp)
    })
  
  result <- do.call("rbind",Vinfo_std_list)
  colnames(result) <- 
    c("Isolate name","Isolate ID","Type","Passage details/history","Lineage","Clade",
      "Collection date","Submitter","Sample ID by sample provider","Sample ID by submitting lab",
      "Last modified","Originating lab","Submitting lab","Gene name","Protein Accession no.","Protein INSDC")
  result <- as.data.frame(result)
  result$FluDB_ID <- GISAID_standardization(result[,1],type=type)
  list(meta=result,sequences=GISAID[[2]])
}
