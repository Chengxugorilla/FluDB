match_Row_Virus <- function(TB_list,GISAID_ref,type){
  d <-
    lapply(seq_along(TB_list),function(i){
      print(i)
      Tb <- TB_list[[i]]
      result <-
        unlist(
          lapply((1:nrow(Tb)),function(j){
            idx <- c()
            ## matched names
            idx_Names <- match_Names(Tb[j,,drop=FALSE],GISAID_ref)
            if(length(idx_Names) == 0)
              return(0)

            ## matched dates
            idx_Date <- idx_Names[match_dates(Tb[j,3],GISAID_ref[idx_Names,])]

            ## matched passages
            idx_Passage <- idx_Names[match_passage(Tb[j,4],GISAID_ref[idx_Names,])]

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
      Tb <- cbind(result,Tb)
      colnames(Tb)[1] <- "GISAID_ID"
      return(Tb)
    })
  names(d) <- names(TB_list)
  return(d)
}
