write_TB_list <- function(TB_list,file_path){
  wb <- openxlsx::createWorkbook()
  for(i in seq_along(TB_list)){
    sheet_name <- names(TB_list)[i]
    df <- TB_list[[i]]
    df$`Collection date` <-  as.character(df$`Collection date`)
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, df)
  }
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}
