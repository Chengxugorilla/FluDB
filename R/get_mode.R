get_mode <- function(x){
  freq_table <- rev(sort(table(x,useNA = "ifany")))
  mode_candidate <- names(freq_table)

  if(length(mode_candidate) > 1 & is.na(mode_candidate[1])){
    mode <- mode_candidate[2]
  }else{
    mode <- mode_candidate[1]
  }

  return(mode)
}
