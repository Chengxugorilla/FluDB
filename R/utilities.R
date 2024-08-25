get_mode <- function(x){
  freq_table <- sort(table(x))
  return(names(freq_table)[1])
}
