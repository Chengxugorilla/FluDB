titer_standardization <- function(TB_list){
  lapply(seq_along(TB_list),function(i){
    print(i)
    tb <- TB_list[[i]][,-(2:5)]

    ## ND
    tb[tb == "ND"] <- 5

    ## <
    tb[tb == "<"] <- 5
    tb[tb == "0"] <- 5
    tb[tb == ">5120"] <- 5120
    tb[tb == ">"] <- 5120

    ## <
    tb[,-1] <-
      apply(tb[,-1], 2, function(x){
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
}
