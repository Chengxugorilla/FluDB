#' @title Check row matched
#' @description check which row in a table with Genetic group do not
#' match to a GISAID sequence.
#' @param TB_list description

check_matched <- function(TB_list){
  mm <-
  lapply(seq_along(TB_list), function(i){
    idx_1 <- which(!is.na(TB_list[[i]]$`Genetic group`))
    idx_2 <- which(TB_list[[i]]$GISAID_ID == 0)
    idx <- intersect(idx_1,idx_2)
    return(TB_list[[i]][idx,,drop=FALSE]$Virus)
  })

  names(mm) <- names(TB_list)
  filtered_list <- Filter(function(x) length(x) > 0, mm)
  return(filtered_list)
}
