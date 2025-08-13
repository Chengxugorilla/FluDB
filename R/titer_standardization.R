titer_standardization <- function(TB_list,match.col,match_start,titer_descent=FALSE){
  result <-
  lapply(seq_along(TB_list),function(i){
    #browser()
    print(i)
    tb <- TB_list[[i]]
    if(match_start > ncol(tb))
      return()

    tb_titer <- tb[,match_start:ncol(tb)-1,drop=FALSE]
    tb_titer[tb_titer == "NT"] <- NA
    ## ND
    tb_titer[tb_titer == "ND"] <- NA

    if(titer_descent){
      idx_20 <- grep("*1", colnames(tb_titer),fixed = TRUE)
      idx_40 <- grep("*2", colnames(tb_titer),fixed = TRUE)
      idx_80 <- grep("*3", colnames(tb_titer),fixed = TRUE)
      if(length(idx_20) > 0){
        temp <- tb_titer[,idx_20]
        temp[temp == "<"] <- 20
        tb_titer[,idx_20] <- temp
      }
      if(length(idx_40) > 0){
        temp <- tb_titer[,idx_40]
        temp[temp == "<"] <- 40
        tb_titer[,idx_40] <- temp
      }
      if(length(idx_80) > 0){
        temp <- tb_titer[,idx_80]
        temp[temp == "<"] <- 80
        tb_titer[,idx_40] <- temp
      }
      tb_titer[tb_titer == "<"] <- 20
    }else{
      tb_titer[tb_titer == "<"] <- 20
    }
    tb_titer[tb_titer == "0"] <- NA
    tb_titer[tb_titer == "60"] <- NA
    tb_titer[tb_titer == "120"] <- NA
    tb_titer[tb_titer == "180"] <- NA
    tb_titer[tb_titer == "440"] <- NA
    tb_titer[tb_titer == "6340"] <- NA
    tb_titer[tb_titer == ">5120"] <- 5120

    tb[,match_start:ncol(tb)-1] <- tb_titer

    ## <
    if(ncol(tb)<=match_start)
      return()
    tb[,match_start:(ncol(tb) - 1)] <-
      apply(tb[,match_start:(ncol(tb) - 1),drop=FALSE], 2, function(x){
        unlist(lapply(seq_along(x), function(i){
          if(is.na(x[i]))
            return(x[i])
          if(stringr::str_detect(x[i],"<")){
            titer <- sub("<","",x[i])
            result <- as.numeric(titer)/2
            return(result)
          }else{
            return(as.numeric(x[i]))
          }
        }))
      })
    return(tb)
  })
  names(result) <- names(TB_list)
  result <- Filter(Negate(is.null), result)
  return(result)
}
