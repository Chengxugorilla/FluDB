#' @title title
#' @description dd
#' @param file description
#' @export

read_WHO_xlsx <- function(file){
  sheet_names <- readxl::excel_sheets(file)
  table_list <-
    lapply(sheet_names,function(x){
      tb <- as.data.frame(readxl::read_excel(file, sheet = x))
      colnames(tb) <- sub("\\.\\.\\.\\d+","",colnames(tb))
      tb
    })
  names(table_list) <- sheet_names
  return(table_list)
}
