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
