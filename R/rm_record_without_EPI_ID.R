#' @title Build GISAID reference information
#' @description return a list contain meta information of virus from a fasta file.
#' @param TB_list description
#' @export

rm_record_without_EPI_ID <- function(TB_list){
  result_list <-
    lapply(seq_along(TB_list), function(i){
      Tb <- TB_list[[i]]
      idx_absent_row <- which(is.na(Tb$Isolate_ID))
      if(length(idx_absent_row) > 0){
        Tb <- Tb[,!colnames(Tb) %in% idx_absent_row]
        Tb <- Tb[-idx_absent_row,]
      }
      return(Tb)
    })
  names(result_list) <- names(TB_list)

  filtered_list <- lapply(result_list, function(df) {
    if (nrow(df) > 0) {
      df
    } else {
      NULL
    }
  })
  filtered_list <- Filter(Negate(is.null), filtered_list)

  return(filtered_list)
}
