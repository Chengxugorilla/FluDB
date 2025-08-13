#' @title Obtain index of GISAID
#' @description A short description...
#' @param tb description
#' @param GISAID_ref description
#' @param type description
#' @export

match_Names <- function(tb,GISAID_ref,type="H3N2"){
  If_nicks <- "Nicknames" %in% names(tb)
  if(length(tb) == 1){
    Names <- tb
    Nicknames <- WHO_standardization(tb,type = type)
  }else{
    Names <- tb$Virus
    if(If_nicks)
      Nicknames <- tb$Nicknames
  }

  ## fully match
  idx_Reals <- unique(c(grep(Names,GISAID_ref[,1],fixed = TRUE),
                        grep(Names,GISAID_ref$FluDB_ID,fixed = TRUE)))
  if(If_nicks){
    idx_Nicks <- unique(c(grep(Nicknames,GISAID_ref[,1],fixed = TRUE),
                          grep(Nicknames,GISAID_ref$FluDB_ID,fixed = TRUE)))
    idx_Virus <- union(idx_Reals,idx_Nicks)
  }else{
    idx_Virus <- idx_Reals
  }

  ## nickname match
  if(length(idx_Virus) == 0){
    Virus_std <- strsplit(Names,"/",fixed = TRUE)[[1]]
    Virus_std_year <- Virus_std[length(Virus_std)]
    if(nchar(Virus_std_year) == 2){
      if(Virus_std_year <= 24)
        Virus_std_year <- paste0("20",Virus_std_year)
      if(Virus_std_year > 24)
        Virus_std_year <- paste0("19",Virus_std_year)
    }else if(nchar(Virus_std_year) != 4){
      message(Names)
    }

    Virus_std[length(Virus_std)] <- Virus_std_year
    Virus_std_Names <- paste(Virus_std,collapse="/")
    idx_Virus <- unique(c(grep(Virus_std_Names,GISAID_ref[,1],fixed = TRUE),
                          grep(Virus_std_Names,GISAID_ref$FluDB_ID,fixed = TRUE)))
  }
  return(idx_Virus)
}
