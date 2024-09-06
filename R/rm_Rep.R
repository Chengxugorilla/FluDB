rm_Rep <- function(TB){
  Vir <- names(table(TB$GISAID_ID)[table(TB$GISAID_ID) > 1])
  if(length(Vir) == 0)
    return(TB)

  replica <-
    lapply(seq_along(Vir), function(i){
      x <- Vir[i]
      rows <- unlist(apply(TB[TB$GISAID_ID == x,-1],2,get_mode))
      if(!is.null(rows)){
        df <- as.data.frame(t(rows))
        df <- cbind(x,df)
        colnames(df)[1] <- "GISAID_ID"
        return(df)
      }
      else
        return()
    })
  names(replica) <- Vir
  replica_list <- Filter(Negate(is.null), replica)
  library(plyr)
  sup <- do.call("rbind.fill",replica_list)
  rownames(sup) <- names(replica_list)
  result <- rbind.fill(TB[!TB$GISAID_ID %in% Vir,],sup)
  return(result[!is.na(result$GISAID_ID),])
}

