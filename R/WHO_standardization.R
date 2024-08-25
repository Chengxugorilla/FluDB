#' @title title
#' @description A short description...
#' @param names description
#' @param type description

WHO_standardization <- function(names,type="H3N2"){
  WHO_sub <-
  switch(type,
         H3N2=WHO_sub_H3N2,
         H1N1=WHO_sub_H1N1,
         all=rbind(WHO_sub_H1N1,WHO_sub_H3N2))
  for (i in seq_along(WHO_sub[,1])) {
    names <- sub(WHO_sub[i,1],WHO_sub[i,2],names,fixed = TRUE)
  }
  for (i in seq_along(WHO_sub_H3N2_reg[,1])) {
    names <- sub(WHO_sub_H3N2_reg[i,1],WHO_sub_H3N2_reg[i,2],names)
  }

  parts <- strsplit(names, "/")

  wrong_format <-
  which(unlist(
  lapply(seq_along(parts),function(i){
    length(parts[[i]])!=4
  })))
  if(length(wrong_format) != 0){
    warning(paste0(paste(names[wrong_format],collapse = "\n"),"in names must have 3 '/' !"))
  }

  years_new <- unlist(
  lapply(seq_along(parts), function(i){
    year <- parts[[i]][length(parts[[i]])]
    if(nchar(year) == 2){
      if(year < 25){
        year <- paste0("20",year)
      }else{
        year <- paste0("19",year)
      }
    }else if(nchar(year) != 4){
      warning(paste0("Year in ",names[i]," is wrong!"))
    }
    year
  }))

  lapply(seq_along(parts), function(i){
    Virus_v <- parts[[i]]
    Virus_v[4] <- years_new[i]
    paste(Virus_v, collapse = "/")
  })
}
