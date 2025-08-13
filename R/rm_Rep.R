#' @title rm
#' @description
#' remove replicated row virus names
#' @param TB description
#' @param match.col description
#' @importFrom dplyr bind_rows

rm_Rep <- function(TB,match.col){
  Vir <- names(table(TB[,match.col])[table(TB[,match.col]) > 1])
  if(length(Vir) == 0)
    return(TB)

  replica <-
    lapply(seq_along(Vir), function(i){
      x <- Vir[i]
      rows <- unlist(apply(TB[TB[,match.col] == x,-1],2,get_mode))
      if(!is.null(rows)){
        df <- as.data.frame(t(rows))
        df <- cbind(x,df)
        colnames(df)[1] <- match.col
        return(df)
      }
      else
        return()
    })
  names(replica) <- Vir
  replica_list <- Filter(Negate(is.null), replica)
  sup <- do.call("bind_rows",replica_list)
  rownames(sup) <- names(replica_list)
  result <- bind_rows(TB[!TB[,match.col] %in% Vir,],sup)
  return(result[!is.na(result[,match.col]),])
}

