titer_standardization <- function(TB_list){
  browser()
  lapply(seq_along(TB_list),function(i){
    tb <- TB_list[[i]]
    apply(tb[,-(1:5)])
  })

  titers
  unlist(
  lapply(seq_along(titers), function(i){
    if(stringr::str_detect(titers[i],"<")){
      titer <- sub("<","",titers[i])
      result <- as.numeric(titer)/2
      return(result)}
    else{
      return(as.numeric(titers[i]))
    }
  }))
}
