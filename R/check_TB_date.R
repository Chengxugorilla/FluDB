#' @title title
#' @description d
#' @param TB_list description
#' @export

check_TB_Date <- function(TB_list){
  #browser()
  error_list <- list()
  TB_Names <- names(TB_list)
  for(i in seq_along(TB_list)){
    tb <- TB_list[[i]]
    checked_date <- check_dates(tb$`Collection date`)
    if(!all(checked_date)){
      error_list[[TB_Names[i]]] <- which(!checked_date)
    }
  }
  error_list
}

#' @title title
#' @description d
#' @param dates check whether is YYYY-MM-DD YYYY-MM YYYY-M
#' @export

check_dates <- function(dates) {
  #tryCatch({
    #browser()
    #dates <- as.Date(dates)
    #dates[is.na(dates)] <- lubridate::ymd_hms("1900-01-01 00:00:00", tz = "UTC")
    # seach pattern
    date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$|^\\d{4}-\\d{2}$|^\\d{4}-\\d{1}$|^\\d{4}-\\d{1}-\\d{2}$|^\\d{4}-\\d{2}-\\d{1}$|^\\d{4}-\\d{1}-\\d{1}$"

    # seach
    result <-
      unlist(lapply(seq_along(dates), function(i){
        date <- dates[i]
        if(grepl(date_pattern, date)){
          TRUE
        } else {
          FALSE
        }
      }))
    return(result)
  #   },
  #   error = function(e){
  #     return(rep(FALSE,length(dates)))
  # })
}
