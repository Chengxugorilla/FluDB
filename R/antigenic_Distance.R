antigenic_Distance <- function(TB_list,match.col){
  lapply(seq_along(TB_list),function(i){
    print(i)
    tb <- rm_Rep(TB_list[[i]],match.col)
    tb <- tb[,colnames(tb) != "Nicknames"]
    if(is.null(tb))
      return()

    ref_V <- colnames(tb)[-(1:6)]

    Ref_Virus <- c()
    Ref_Ferret <- c()
    for(i in seq_along(ref_V)){
      Ref_Virus <- c(Ref_Virus, strsplit(ref_V," ")[[i]][1])
      Ref_Ferret <- c(Ref_Ferret, strsplit(ref_V," ")[[i]][2])
    }

    test <-
    lapply(2:ncol(tb),function(i){
      Ref_Virus <- rep(Ref_Virus[i-1],nrow(tb))
      Test_Virus <- tb[,1]
      Ferret_ID <- rep(Ref_Ferret[i-1],nrow(tb))
      HI_titer <- tb[,i]

      return(data.frame(Ref_Virus, Test_Virus, Ferret_ID, HI_titer))
    })
    ttest <- do.call("rbind", test)

    distance_list <- list()
    list_names <- c()
    for (vid in Ref_Virus) {
      print(vid)
      ref_col <- grep(vid, colnames(tb))[1]
      ref_titer <- as.numeric(get_mode(tb[tb[,match.col] == vid, ref_col]))
      test_titer <- as.numeric(tb[tb[,match.col] != vid, ref_col])
      if(length(ref_titer)==0|length(test_titer)==0)
        next

      distance <- unlist(
        lapply(seq_along(test_titer),function(i){
          if(is.na(test_titer[i]))
            return(NA)
          distance_ab <- log2(ref_titer) - log2(test_titer[i])
          return(distance_ab)
        }))

      names(distance) <- tb[,match.col][tb[,match.col] != vid]
      distance <- distance[!is.na(distance)]
      distance_list <- append(distance_list,list(distance))
      list_names <- c(list_names,vid)
    }
    names(distance_list) <- list_names
    distance_list <- distance_list[!is.na(distance_list)]
    return(distance_list)
    })
}
