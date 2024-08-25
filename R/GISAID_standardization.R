#' @title title
#' @description A short description...
#' @param names description
#' @param type description

GISAID_standardization <- function(names,type="H3N2"){
  GISAID_sub <-
    switch(type,
           H1N1=GISAID_sub_H1N1,
           H3N2=GISAID_sub_H3N2,
           all=rbind(GISAID_sub_H1N1,GISAID_sub_H3N2))

  for (i in seq_along(GISAID_sub[,1])) {
    names <- sub(GISAID_sub[i,1],GISAID_sub[i,2],names,fixed = TRUE)
  }
  for (i in seq_along(GISAID_sub_H3N2_reg[,1])) {
    names <- sub(GISAID_sub_H3N2_reg[i,1],GISAID_sub_H3N2_reg[i,2],names)
  }
  names
}
