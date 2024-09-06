col_search <- function(Virus,GISAID_ref,type){
  if(startsWith(Virus,"EPI")){ ## if Virus are GISAID_id
    idx <- which(GISAID_ref$`Isolate ID` == Virus)
    if(length(idx) != 1){
      stop("in col_search function, a EPI_ISL id corresponding to multiple GISAID record.")
    }
    return(idx)
  }
  
  strings <- stringr::str_extract(Virus,"(A|B)/.*/([0-9]{4}|[0-9]{2})")
  
  idx_Names <- match_Names(strings[1], GISAID_ref,type)
  if(length(idx_Names) == 0)
    return(0)
  
  idx_Passage <- idx_Names[match_passage(strings[2],GISAID_ref[idx_Names,])]
  
  if(length(idx_Passage)==0){
    idx <- idx_Names
  }else{
    idx <- idx_Passage
  }
  return(idx[1])
}