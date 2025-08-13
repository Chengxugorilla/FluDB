#' @title exact match strain from GISAID
#' @description
#' 在GISAID参考信息中,寻找Virus name, Collection date, Passage history完全一样的序列,并匹配EPI_ISL_ID并写入文件
#' @param absent_Isolates description
#' @param GISAID_REF description
#' @param save_path description

exact_matching_strain <- function(absent_Isolates, GISAID_REF, save_path){
  ## 在GISAID参考信息中,寻找Virus name, Collection date, Passage history完全一样的序列
  warning_list <- c()
  consider_Data <- FALSE
  ## 病毒名全部变成大写，避免大小写不匹配
  GISAID_REF$`Isolate name` <- toupper(GISAID_REF$`Isolate name`)
  for(i in seq_along(absent_Isolate[,1])){
    Record <- absent_Isolate[i,]
    Virus <- Record$Virus
    Date <- Record$`Collection date`
    Passage <- Record$`Passage history`

    idx_Virus <- which(GISAID_REF$`Isolate name` == toupper(Virus))

    idx_Date <- which(GISAID_REF$`Collection date` == Date)
    if(is.na(Date))
      idx_Date <- seq_along(GISAID_REF[,1])
    idx_Passage <- which(GISAID_REF$`Passage details/history` == Passage)
    if(is.na(Passage))
      idx_Passage <- seq_along(GISAID_REF[,1])
    if(consider_Data)
      idx_match <- intersect(intersect(idx_Virus, idx_Date),idx_Passage)
    else
      idx_match <- intersect(idx_Virus,idx_Passage)

    if(length(idx_match) != 0){
      print(i)
      absent_Isolate[i,1:2] <- c(GISAID_REF$`Isolate ID`[idx_match[1]],'same')
    }else if(length(idx_match) > 1){
      warning_list <- c(warning_list,i)
    }
  }
  idx_1 <- which(is.na(absent_Isolate$`Genetic group`))
  idx_2 <- which(is.na(absent_Isolate$Isolate_ID))
  if(length(idx_1)!=0&length(idx_2)!=0){
    dd=absent_Isolate[-intersect(idx_1,idx_2),]
  }else{
    dd=absent_Isolate
  }
  dd$Isolate_ID[is.na(dd$Isolate_ID)] <- ""
  dd$ID_Passs[is.na(dd$ID_Passs)] <- ""

  return(dd)
}
