match_Col_Virus <- function(TB_list,GISAID_ref,type){
  New_TB <-
    lapply(seq_along(TB_list),function(i){
      print(i)
      Tb <- TB_list[[i]]
      colnames(Tb)[1] <- "GISAID_ID"

      Ref_Virus <- colnames(Tb)[6:(ncol(Tb)-2)]
      ## turn numeric to GISAID column number
      numeric_idx <- stringr::str_detect(Ref_Virus, "^\\d+$")
      colnames(Tb)[6:(ncol(Tb)-2)][numeric_idx] <- Tb$GISAID_ID[as.numeric(Ref_Virus[numeric_idx])]

      ## search character name from GISAID
      Search_list <- Ref_Virus[!numeric_idx]
      colnames(Tb)[6:(ncol(Tb)-1)][!numeric_idx] <- 
        unlist(lapply(Search_list, col_search,GISAID_ref=GISAID_ref,type=type))

      colnames(Tb)[is.na(colnames(Tb))] <- 0
      Vir <- names(table(colnames(Tb))[table(colnames(Tb)) > 1])

      if(length(Vir) > 0){
        test <-
          lapply(Vir, function(x){
            apply(Tb[,colnames(Tb) == x],1,get_mode)
          })
        sup <- as.data.frame(do.call(cbind,test))
      }

      if(exists("sup")){
        colnames(sup) <- Vir
        Tb <- cbind(Tb[,!colnames(Tb) %in% Vir],sup)
        Tb <- Tb[,!colnames(Tb) == "Type"]
      }else{
        Tb <- Tb[,-ncol(Tb)]
        Tb <- Tb[,!colnames(Tb) == "Type"]
      }
      Tb <- Tb[Tb$GISAID_ID != 0,colnames(Tb) != 0]
      Tb <- Tb[!is.na(Tb$GISAID_ID),]
      return(Tb)
    })
  return(New_TB)
}
