#' @title Build GISAID reference information
#' @description return a list contain meta information of virus from a fasta file.
#' @param file the address of the fasta file.
#' @param type flu type
#' @param seq_type DNA or AA.
#' @param nROW rows in df
#' @export

build_GISAID <- function(file,type,seq_type,nROW){
  ## read sequence from file
  #browser()
  GISAID <- read_sequences(file,seq_type)
  Vinfo_list <- strsplit(GISAID[[1]],"|",fixed = TRUE)

  if(seq_type == "AA"){
    Vinfo_std_list <-
      lapply(seq_along(Vinfo_list), function(i){
        temp <- Vinfo_list[[i]]
        if(length(temp) == (nROW - 1)){
          temp <- c(temp,NA)
        }else if(length(temp) != nROW){
          warning(temp[1])
        }
        return(temp)
      })
  }else{
    Vinfo_std_list <-
      lapply(seq_along(Vinfo_list), function(i){
        temp <- Vinfo_list[[i]]
        if(length(temp) == 5){
          temp <- c(temp,NA)
        }else if(length(temp) != 6){
          warning(temp[1:2])
        }
        return(temp)
      })
  }

  unlist(lapply(seq_along(Vinfo_std_list), function(i){
    if(length(Vinfo_std_list[[i]])!=nROW)
      print(Vinfo_std_list[[i]][2])
  }))
  result <- do.call("rbind",Vinfo_std_list)
  colnames(result) <- switch(seq_type,
                             DNA = c("Isolate name",
                                     "Isolate ID",
                                     "Passage details/history",
                                     "Clade",
                                     "Collection date",
                                     "DNA Accession no."),
                             # AA = c("Isolate name",
                             #        "Isolate ID",
                             #        "Passage details/history",
                             #        "Clade",
                             #        "Collection date",
                             #        "DNA Accession no."))
                             AA = c("Isolate name","Isolate ID","Type",
                                    "Passage details/history","Lineage","Clade",
                                    "Collection date","Submitter","Sample ID by sample provider",
                                    "Sample ID by submitting lab","Last modified",
                                    "Originating lab","Submitting lab","Gene name",
                                    "Protein Accession no.","Protein INSDC"))

  result <- as.data.frame(result)
  result$FluDB_ID <- GISAID_standardization(result[,1],type=type)
  list(meta=result,sequences=GISAID[[2]])
}
