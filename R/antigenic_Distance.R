antigenic_Distance <- function(TB_list){
  lapply(seq_along(TB_list),function(i){
    print(i)
    tb <- rm_Rep(TB_list[[i]])
    tb <- tb[,colnames(tb) != "Nicknames"]

    ref_V <- colnames(tb)[-1]
    ref_V <- ref_V[ref_V %in% tb$GISAID_ID]

    distance_list <- list()
    for (vid in ref_V) {
      ref_col <- which(colnames(tb) == vid)
      ref_titer <- as.numeric(get_mode(tb[tb$GISAID_ID == vid, ref_col]))
      if(is.na(ref_titer))
        return(rep(NA,length(test_titer)))
      test_titer <- as.numeric(tb[tb$GISAID_ID != vid, ref_col])
      distance <- unlist(
      lapply(seq_along(test_titer),function(i){
        if(is.na(test_titer[i]))
          return(NA)
        distance_ab <- log2(ref_titer) - log2(test_titer[i])
        return(distance_ab)
      }))
      names(distance) <- tb$GISAID_ID[tb$GISAID_ID != vid]
      distance <- distance[!is.na(distance)]
      distance_list <- append(distance_list,list(distance))
    }
    names(distance_list) <- ref_V
    distance_list <- distance_list[!is.na(distance_list)]
    return(distance_list)
    })
}
