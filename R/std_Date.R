#' @title title
#' @description d
#' @param TB_list description
#' @export

std_Date <- function(TB_list){
  result <-
  lapply(seq_along(TB_list),function(i){
    tb <- TB_list[[i]]
    tb$`Collection date` <- as.Date(std_Date_tb(tb))
    return(tb)
  })
  names(result) <- names(TB_list)
  return(result)
}

std_Date_tb <- function(table){
  dates <- table$`Collection date`
  ct <- stringr::str_count(dates,"-")
  no_day_idx <- ct == 1
  no_day_idx[is.na(no_day_idx)] <- FALSE
  if(any(no_day_idx))
    dates[no_day_idx] <- paste0(dates[no_day_idx],"-01")

  return(dates)
}
