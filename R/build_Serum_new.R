#' @title title
#' @description d
#' @param TB_list description
#' @param GISAID_ref description
#' @param type description
#' @export

build_Serum_new <- function(TB_list,GISAID_ref,type){
  ## Searching matched sequences in GISAID
  d <-
    lapply(seq_along(TB_list),function(i){
      print(i)
      tb <- TB_list[[i]]
      result <-
        unlist(
          lapply((1:nrow(tb)),function(j){
            idx <- c()
            ## matched names
            idx_Names <- match_Names(tb[j,,drop=FALSE],GISAID_ref)
            if(length(idx_Names) == 0)
              return(0)

            ## matched dates
            idx_Date <- idx_Names[match_dates(tb[j,3],GISAID_ref[idx_Names,])]

            ## matched passages
            idx_Passage <- idx_Names[match_passage(tb[j,4],GISAID_ref[idx_Names,])]

            if(length(idx_Date)==0 & length(idx_Passage)==0){
              idx <- idx_Names
            }else if(length(idx_Date)!=0 & length(idx_Passage)!=0){
              idx <- intersect(idx_Date,idx_Passage)
            }else if(length(idx_Date)!=0){
              idx <- idx_Date
            }else if(length(idx_Passage)!=0){
              idx <- idx_Passage
            }else{
              idx <- idx_Names
            }

            if(length(idx)==0)
              idx <- idx_Names

            return(idx[1])
          }))
      result
    })
  names(d) <- names(TB_list)

  New_TB <-
    lapply(seq_along(d),function(i){
      Tb <- cbind(d[[i]],TB_list[[i]])
      colnames(Tb)[1] <- "GISAID_ID"

      Ref_Virus <- colnames(Tb)[6:(ncol(Tb)-1)]
      ## turn numeric to GISAID column number
      numeric_idx <- stringr::str_detect(Ref_Virus, "^\\d+$")
      colnames(Tb)[6:(ncol(Tb)-1)][numeric_idx] <- Tb$GISAID_ID[as.numeric(Ref_Virus[numeric_idx])]

      ## search character name from GISAID
      Search_list <- Ref_Virus[!numeric_idx]
      colnames(Tb)[6:(ncol(Tb)-1)][!numeric_idx] <- unlist(lapply(Search_list, col_search,GISAID_ref=GISAID_ref,type=type))

      colnames(Tb)[is.na(colnames(Tb))] <- 0
      Vir <- names(table(colnames(Tb))[table(colnames(Tb)) > 1])

      test <-
        lapply(Vir, function(x){
          unlist(
            apply(Tb[,colnames(Tb) == x],1,get_mode))
        })
      sup <- do.call(cbind,test)
      if(!is.null(sup)){
        colnames(sup) <- Vir
        Tb <- cbind(Tb[,!colnames(Tb) %in% Vir],sup)
        Tb <- Tb[,!colnames(Tb) == "Type"]
      }else{
        Tb <- Tb[,-ncol(Tb)]
      }
      Tb <- Tb[Tb$GISAID_ID != 0,colnames(Tb) != 0]
      Tb <- Tb[!is.na(Tb$GISAID_ID),]
      return(Tb)
    })
  return(New_TB)
}


#' @title Obtain index of GISAID
#' @description A short description...
#' @param Dates description
#' @param GISAID_ref description
#' @export

match_dates <- function(Dates,GISAID_ref){
  if(is.na(Dates))
    return(which(is.na(GISAID_ref[,3])))

  idx_Date <- grep(Dates,GISAID_ref[,3],fixed = TRUE)
  return(idx_Date)
}


#' @title Obtain index of GISAID
#' @description A short description...
#' @param Passages description
#' @param GISAID_ref description
#' @export

match_passage <- function(Passages,GISAID_ref){
  ## if Passages is NA
  if(is.na(Passages)){
    idx_na <- which(is.na(GISAID_ref[,2]))
    if(length(idx_na) == 0)
      idx_na <- 0
    return(idx_na)
  }

  ## fully matched
  idx_Passage <- grep(Passages,GISAID_ref[,2],fixed = TRUE)

  ## CELL/EGG matched
  if(length(idx_Passage) == 0){
    idx_Passage <- which(unlist(
    lapply(seq_along(GISAID_ref[,2]), function(i){
      passage_ident(Passages,GISAID_ref[i,2])
    })))
  }
  # WHO_attri <- passage_attribute(Passages)

  return(idx_Passage)
}

passage_ident <- function(P1,P2){
  pi(P1) == pi(P2)
}
pi <- function(P){
  if(grepl("MDCK|SIAT|C|M|S",P)){
    return('CELL')
  }
  else if(grepl("E|cs",P)){
    return('EGG')
  }else if(grepl("Original",P)){
    return('Origin')
  }else{
    warning(P)
    return()
  }
}

# passage_attribute <- function(passage){
#   P_string <- strsplit(passage,split = '/')[[1]]
#   P_history <- list(MDCK=c(),SIAT=c(),C=c(),E=c(),D=c())
#
#   letters_part <- stringr::str_extract(P_string, "\\D+")
#   numbers_part <- as.numeric(stringr::str_extract(P_string, "\\d+"))
#   if(length(letters_part) != length(numbers_part))
#     return(P_history)
#
#   for (i in seq_along(letters_part)) {
#     p <- letters_part[i]
#     generation <- numbers_part[i]
#     if(p=='MDCK')
#       P_history[['MDCK']] <- c(P_history[['MDCK']],generation)
#     if(p=='SIAT')
#       P_history[['SIAT']] <- c(P_history[['SIAT']],generation)
#     if(p=='C')
#       P_history[['C']] <- c(P_history[['C']],generation)
#     if(p=='E')
#       P_history[['E']] <- c(P_history[['E']],generation)
#     if(p=='D')
#       P_history[['D']] <- c(P_history[['D']],generation)
#   }
#   return(P_history)
# }
#
# passage_distance <- function(passage_str){
#   P1 <- strsplit(passage_str[1],split = '/')[[1]]
#   P2 <- strsplit(passage_str[2],split = '/')[[1]]
#
# }
