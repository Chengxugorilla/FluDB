#' @title Obtain index of GISAID
#' @description A short description...
#' @param Names description
#' @param GISAID_ref description
#' @param type description
#' @export

match_Names <- function(tb,GISAID_ref,type="H3N2"){
  if(length(tb) == 1){
    Names <- tb
    Nicknames <- WHO_standardization(tb,type = type)
  }else{
    Names <- tb$Virus
    Nicknames <- tb$Nicknames
  }

  ## fully match
  idx_Reals <- unique(c(grep(Names,GISAID_ref[,1],fixed = TRUE),
                        grep(Names,GISAID_ref$FluDB_ID,fixed = TRUE)))
  idx_Nicks <- unique(c(grep(Nicknames,GISAID_ref[,1],fixed = TRUE),
                        grep(Nicknames,GISAID_ref$FluDB_ID,fixed = TRUE)))
  idx_Virus <- union(idx_Reals,idx_Nicks)

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

fuzzy_match <- function(Pattern,Reference){
  H1 <- stringr::str_extract(Pattern,"A/[^/]+/")
  H2 <- stringr::str_extract(Reference,"A/[^/]+/")
  T1 <- stringr::str_replace(Pattern,"A/[^/]+/","")
  T2 <- stringr::str_replace(Reference,"A/[^/]+/","")

  if(T1 == T2){
    ratio <- stringdist::stringdist(H1, H2, method = "lv")/nchar(H1)
    result <- ifelse(ratio < 0.2,TRUE,FALSE)
  }else{
    result <- FALSE
  }
  return(result)
}
