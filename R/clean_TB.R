#' @title title
#' @description d
#' @param TB d

clean_TB <- function(TB){
  lapply(TB,function(x){
    Test_idx <- which(grepl("Test|TEST", x$Virus))
    x$Type <- c(rep("Reference",Test_idx),
                rep("Test",nrow(x)-Test_idx))
    x[-Test_idx,]
  })
}
