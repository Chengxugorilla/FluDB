#' @title Obtain index of GISAID
#' @description A short description...
#' @param Dates description
#' @param GISAID_ref description
#' @export

match_dates <- function(Dates,GISAID_ref){
  if(is.na(Dates))
    return(which(is.na(GISAID_ref[,"Collection date"])))

  idx_Date <- grep(Dates,GISAID_ref[,"Collection date"],fixed = TRUE)
  return(idx_Date)
}
