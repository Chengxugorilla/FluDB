find_absent_from_GISAID <- function(absent_Record,GISAID_REF,idx2match=c(3,5,6)){
  warning_list <- c()
  for(i in seq_along(absent_Record[,1])){
    Record <- absent_Record[i,]
    Virus <- Record[,idx2match[1]]
    Date <- Record[,idx2match[2]]
    Passage <- Record[,idx2match[3]]

    idx_Virus <- which(GISAID_REF[[1]]$`Isolate name` == Virus)
    idx_Date <- which(GISAID_REF[[1]]$`Collection date` == Date)
    idx_Passage <- which(GISAID_REF[[1]]$`Passage details/history` == Passage)
    idx_match <- intersect(intersect(idx_Virus,idx_Date),idx_Passage)
    if(length(idx_match) != 0){
      print(i)
      absent_Record[i,1:2] <- c(GISAID_REF[[1]]$`Isolate ID`[idx_match[1]],'same')
    }else if(length(idx_match) > 1){
      warning_list <- c(warning_list,i)
    }
  }
  return(absent_Record)
}
