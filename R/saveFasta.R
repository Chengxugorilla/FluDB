saveFasta <- function(path,table,GISAID_ref){
  GISAID_ID <- sort(union(rownames(table),colnames(table)[-1]))
  seqs <- GISAID_ref[[2]][as.numeric(GISAID_ID)]

  element_to_insert <- paste0(">",GISAID_ref[[1]]$`Isolate name`[as.numeric(GISAID_ID)])
  result_vector <- unlist(sapply(seq_along(seqs), function(i) c(paste0(">",i), seqs[i])))

  writeLines(result_vector,con = path)
  return("Mission success~")
}
