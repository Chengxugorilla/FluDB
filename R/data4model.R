data4model <- function(dist_list,GISAID_ref,type){
  reference_id <- c()
  reference_seq <- c()
  candidate_id <- c()
  candidate_seq <- c()
  value <- c()

  Ref <- names(dist_list)
  for(i in seq_along(Ref)){
    Test <- dist_list[[i]]
    reference_id <- c(reference_id,rep(paste0(type,Ref[i]),length(dist_list[[i]])))
    reference_seq <- c(reference_seq,rep(GISAID_ref[[2]][as.numeric(Ref[i])],length(Test)))
    candidate_id <- c(candidate_id,paste0(type,names(dist_list[[i]])))
    candidate_seq <- c(candidate_seq,GISAID_ref[[2]][as.numeric(names(Test))])
    value <- c(value,Test)
  }
  result <- data.frame(reference_id,reference_seq,candidate_id,candidate_seq,value)
}
