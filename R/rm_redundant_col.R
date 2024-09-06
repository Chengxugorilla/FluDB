rm_redundant_col <- function(TB_list){
  lapply(seq_along(TB_list), function(i){
    tb <- TB_list[[i]]
    tb[,colnames(tb) != "Nicknames"]
  })
}
