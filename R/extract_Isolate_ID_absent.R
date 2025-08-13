extract_Isolate_ID_absent <- function(TB_list,absent.row,info_col,only_Ref){
  result_TB_list <-
  lapply(seq_along(TB_list), function(i){
    print(i)
    tb <- TB_list[[i]][,info_col]
    if(only_Ref)
      tb <- tb[TB_list[[i]]$Type=="Reference",]
    absent_idx <- which(is.na(tb[,absent.row]))
    return(tb[absent_idx,])
  })
  result_table <- do.call("rbind",result_TB_list)
  result_table <- result_table[!duplicated(result_table),]
  return(result_table)
}
