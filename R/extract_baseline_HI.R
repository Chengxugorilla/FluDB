extract_baseline_HI <- function(TB_list,type,match.col,match_start){
  self_HI <- c()
  for(i in seq_along(TB_list)){
    print(i)
    tb <- TB_list[[i]]
    Serum <- colnames(tb)[match_start:ncol(tb)]

    for (Sr in seq_along(Serum)) {
      idx_row <- which(tb[,match.col] == Serum[Sr])
      if(length(idx_row) != 0){
        temp_HI <- tb[,match_start:ncol(tb)]
        if(is.null(dim(temp_HI))){
          self_HI <- c(self_HI,temp_HI)
          names(self_HI)[length(self_HI)] <- Serum[Sr]
          next
        }
        self_HI <- c(self_HI,tb[,match_start:ncol(tb)][idx_row,Sr])
        names(self_HI)[length(self_HI)] <- Serum[Sr]
      }
    }
  }
  self_HI <- self_HI[names(self_HI)!=""]
  Serum_ID <- unique(names(self_HI))

  Serum_HI <- unlist(
    lapply(Serum_ID,function(x){
      mean_titer <- 2^mean(log2(as.numeric(self_HI[names(self_HI) == x])),na.rm=TRUE)
      result <- mean_titer

      return(ifelse(length(result)==0,NA,result))
    }))
  names(Serum_HI) <- paste0(type,Serum_ID)
  return(Serum_HI[!is.na(Serum_HI)])
}

