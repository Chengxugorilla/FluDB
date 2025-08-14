

rm_record_without_EPI_ID <- function(TB_list){
  result_list <-
    lapply(seq_along(filt_1), function(i){
      Tb <- filt_1[[i]]
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
