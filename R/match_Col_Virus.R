match_Col_Virus <- function(TB_list,GISAID_ref,type,parallel=TRUE,match.col,match_start,match_end){
  handlers(global = TRUE)
  New_TB <- progressr::with_progress({
    p <- progressr::progressor(steps = length(TB_list))

    if (parallel) {
      plan(multisession)
    } else {
      plan(sequential)
    }

    lapply(seq_along(TB_list),function(i){
      tryCatch({
        Tb <- TB_list[[i]]

        Ref_Virus <- colnames(Tb)[match_start:(ncol(Tb) + match_end)]
        ## turn numeric to GISAID column number
        numeric_idx <- stringr::str_detect(Ref_Virus, "^\\d+$")
        colnames(Tb)[match_start:(ncol(Tb) + match_end)][numeric_idx] <- Tb[,match.col][as.numeric(Ref_Virus[numeric_idx])]

        ## search character name from GISAID
        Search_list <- Ref_Virus[!numeric_idx]
        colnames(Tb)[match_start:(ncol(Tb) + match_end)][!numeric_idx] <-
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
        p()
        return(Tb)
      }, error =function(e){
        message(sprintf("The %d has some problems: %s", i, e$message))
        p()
        return(NULL)
      })
    })
  })

  return(New_TB)
}
