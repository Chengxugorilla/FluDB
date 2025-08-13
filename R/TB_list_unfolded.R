utils::globalVariables(".")
#' @importFrom magrittr %>%

TB_list_unfolded <- function(TB_list,GISAID_ref,type,parallel=TRUE,match.col,match_start){
  integrate_tb_list <-
    lapply(seq_along(TB_list),function(i){
      # browser()
      print(i)
      Tb <- TB_list[[i]]

      col_Virus_info <-
        colnames(Tb)[match_start:ncol(Tb)] %>%
        strsplit(.," ")

      Ref_Virus <- c()
      Ref_Ferret <- c()
      for(ncol_V in seq_along(col_Virus_info)){
        Ref_Virus <- c(Ref_Virus,col_Virus_info[[ncol_V]][1])
        Ref_Ferret <- c(Ref_Ferret,col_Virus_info[[ncol_V]][2])
      }

      integrate_tb_list <- list() ##list of all serum in a table
      for (Ref_V_idx in seq_along(Ref_Virus)) {
        #print(Ref_V_idx)
        Ref_V <- Ref_Virus[Ref_V_idx]
        is.row_num <- stringr::str_detect(Ref_V, "^\\d+$")
        if(Ref_V %in% rownames(Tb) & is.row_num){
          serum_info <- c(Tb[Ref_V, "Virus"], #serumName
                          Tb[Ref_V, "Passage history"], #serumPassage
                          ifelse(is.null(Tb[Ref_V, "Collection date"]),
                                 NA,Tb[Ref_V, "Collection date"]), #serumDate
                          Ref_Ferret[Ref_V_idx], #ferret
                          Tb[Ref_V, "Isolate_ID"], #serumIslDB
                          Tb[Ref_V, "ID_Passs"]) #serumMatchedPass
          #Ref_HI <- as.numeric(Tb[Ref_V, match_start-1+which(Ref_Virus == Ref_V)])
          Ref_HI <- as.numeric(Tb[Ref_V, match_start-1+Ref_V_idx])
        }else{next}

        Virus_df <- t(
        apply(Tb[rownames(Tb) != Ref_V,],1,function(x){
          #browser()
          virus_info <- c(x["Virus"], #virusName
                          x["Passage history"], #virusPassage
                          x["Collection date"], #virusDate
                          x["Isolate_ID"], #virusIslDB
                          x["ID_Passs"], #virusMatchedPass
                          log2(Ref_HI) - log2(as.numeric(x[match_start-1+Ref_V_idx]))) #HI distance
          return(virus_info)
        }))
        if(ncol(Virus_df)==0)
          next
        colnames(Virus_df) <- c("virusName","virusPassage",
                                "virusDate","virusIslID",
                                "virusMatchedPass","HI_Dist")

        Serum_matrix <-
        matrix(rep(serum_info, nrow(Virus_df)),
               nrow = nrow(Virus_df), byrow = TRUE)
        colnames(Serum_matrix) <- c("serumName","serumPassage","serumDate",
                                    "ferret","serumIslID","serumMatchedPass")
        Serum_df <- as.data.frame(Serum_matrix)
        integrate_tb_list[[length(integrate_tb_list)+1]] <- cbind(Serum_df,Virus_df)
      }

      if(length(integrate_tb_list)==0)
        return()

      table_integrate <- do.call("rbind", integrate_tb_list)
      table_integrate$sheet<- rep(names(TB_list)[i],nrow(table_integrate))
      return(table_integrate)
    })
  result_table <- do.call("rbind",integrate_tb_list)
  rownames(result_table) <- NULL
  return(result_table)
}
