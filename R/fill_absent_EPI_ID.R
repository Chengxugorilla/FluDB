fill_absent_EPI_ID <- function(TB_list,absent_Isolate_clear,idx2match=c(3,5,6)){
  result <-
  lapply(seq_along(TB_list),function(i){
    #browser()
    tb <- TB_list[[i]]
    for (j in seq_along(tb[,1])){
      Record <- tb[j,]
      Virus <- Record[,idx2match[1]]
      Date <- Record[,idx2match[2]]
      Passage <- Record[,idx2match[3]]

      idx_Virus <- which(absent_Isolate_clear$Virus == Virus)
      idx_Date <- which(absent_Isolate_clear$`Collection date` == Date)
      idx_Passage <- which(absent_Isolate_clear$`Passage history` == Passage)
      idx_match <- intersect(intersect(idx_Virus,idx_Date),idx_Passage)
      if(length(idx_match) != 0){
        tb[j,1:2] <- c(absent_Isolate_clear$Isolate_ID[idx_match[1]],absent_Isolate_clear$ID_Passs[idx_match[1]])
      }
    }
    return(tb)
  })
  names(result) <- names(TB_list)
  return(result)
}
