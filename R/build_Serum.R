#' @title title
#' @description d
#' @param TB_list description
#' @param GISAID_ref description

build_Serum <- function(TB_list,GISAID_ref){
  Tb_clean1 <-
    lapply(TB_list,function(x){
      Test_idx <- which(grepl("Test|TEST", x$Virus))
      x$Type <- c(rep("Reference",Test_idx),
                  rep("Test",nrow(x)-Test_idx))
      x[-Test_idx,]
    })

  # find the virus whose '/' not equal to 3
  vnames <- unlist(
    lapply(Tb_clean1,function(x){
      x$Virus
    }))
  idx_wrong <- which(sapply(vnames, function(s){
    stringr::str_count(s,"/") != 3
    }))
  if(length(idx_wrong)!=0)
    warning(paste(vnames[idx_wrong],"\n"))

  Tb_clean2 <-
  lapply(seq_along(Tb_clean1), function(i){
    Tb <- Tb_clean1[[i]]
    Ref_num <- length(which(Tb$Type=="Reference"))
    col_Virus <- colnames(Tb)[5:(ncol(Tb)-1)]
    if(Ref_num==length(col_Virus)){
      col_Virus <- 1:Ref_num
    }else if(all(nchar(col_Virus)!=0)){

    }else if(length(which(nchar(col_Virus)==0))==Ref_num){
      col_Virus[which(nchar(col_Virus)==0)] <- 1:Ref_num
    }
    colnames(Tb)[5:(ncol(Tb)-1)] <- col_Virus
    Tb
  })

  ## Searching matched sequences in GISAID
  d <-
    lapply(seq_along(Tb_clean2),function(i){
      tb <- Tb_clean2[[i]]
      result <-
        unlist(
          lapply((1:nrow(tb)),function(j){
            idx <- 0
            idx_Virus <- GISAID_Virus_idx(tb[j,1],GISAID_ref)
            if(!is.na(tb[j,3])){
              idx_Date <- grep(tb[j,3],GISAID_ref[idx_Virus,3],fixed = TRUE)
            }else{
              idx_Date <- seq_along(idx_Virus)
            }
            if(!is.na(tb[j,4])){
              idx_Passage <- grep(tb[j,4],GISAID_ref[idx_Virus[idx_Date],2],fixed = TRUE)
            }else{
              idx_Passage <- integer(0)
            }
            if(length(idx_Passage)!=0){
              idx <- idx_Virus[idx_Date[idx_Passage]]
            }else if(length(idx_Date)!=0){
              idx <- idx_Virus[idx_Date]
            }else if(length(idx_Virus)!=0){
              idx <- idx_Virus
            }
            return(idx[1])
          }))
      result
    })

  New_TB <-
  lapply(seq_along(d),function(i){
    Tb <- cbind(d[[i]],Tb_clean2[[i]])
    colnames(Tb)[[1]] <- "GISAID_ID"
    use_idx <- as.numeric(colnames(Tb)[6:(ncol(Tb)-1)])
    colnames(Tb)[6:(ncol(Tb)-1)] <- Tb$GISAID_ID[use_idx]
    Tb
  })

  test <- unlist(
  lapply(seq_along(New_TB),function(i){
    any(is.na(colnames(New_TB[[i]])))
  }))
  return(New_TB)
 }
