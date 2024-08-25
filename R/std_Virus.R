#' @title title
#' @description d
#' @param TB_list description
#' @export

std_Virus <- function(TB_list){
  # remove mutation information
  Mut_free <-
  lapply(seq_along(TB_list), function(i){
    tb <- TB_list[[i]]
    rm_mutate(tb)
  })
  names(Mut_free) <- names(TB_list)
  return(Mut_free)
}

rm_mutate <- function(table){
  Virus <- table$Virus
  table$Virus <- stringr::str_extract(Virus, "A/.*/(\\d{2}|\\d{4})$")
  return(table)
}
