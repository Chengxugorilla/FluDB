list2big_table <- function(TB_list,match.col){
  final_list <-
  lapply(seq_along(TB_list),function(i){
    print(i)
    tb <- rm_Rep(TB_list[[i]],match.col)
    tb <- tb[,colnames(tb) != "Nicknames"]
    if(is.null(tb))
      return()

    ref_V <- colnames(tb)[-1]

    Ref_Virus <- c()
    Ref_Ferret <- c()
    Ref_info <- strsplit(ref_V, " ")
    for(j in seq_along(ref_V)){
      Ref_Virus <- c(Ref_Virus, Ref_info[[j]][1])
      Ref_Ferret <- c(Ref_Ferret, Ref_info[[j]][2])
    }

    column_list <-
      lapply(2:ncol(tb),function(k){
        Serum <- rep(Ref_Virus[k-1],nrow(tb))
        Virus <- tb[,1]
        Ferret <- rep(Ref_Ferret[k-1],nrow(tb))
        HI_titer <- tb[,k]
        return(data.frame(Serum, Virus, Ferret, HI_titer))
      })

    result_tb <- do.call("rbind", column_list)
    result_tb$source <- names(TB_list)[i]
    return(result_tb)
  })

  final_table <- do.call("rbind", final_list)

  return(final_table)
}
