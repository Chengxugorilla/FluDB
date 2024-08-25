#' @title title
#' @param GISAID description
#' @param WHO description

Generate_json <- function(WHO, GISAID, path){
  ## rows and columns GISAID_ID
  idx_cols <- colnames(WHO)[-1]
  idx_rows <- WHO$GISAID_ID

  Ref_vector <- Generate_json_vector(idx_cols,GISAID)
  Test_vector <- Generate_json_vector(idx_rows,GISAID)

  Ref_path <- paste0(path, "_Ref.json")
  Test_path <- paste0(path, "_Test.json")
  writeLines(Ref_vector,Ref_path)
  writeLines(Test_vector,Test_path)
  return()
}

Generate_json_vector <- function(idx,GISAID){
  json_list <-
    lapply(seq_along(idx), function(i){
      paste0('"',idx[i],'":"',tolower(GISAID[[2]][i]),'",')
    })
  ## replace ,
  json_list[[length(json_list)]] <- sub(",","}",json_list[[length(json_list)]])
  json_vector <- unlist(json_list)
  result <- c("{",json_vector)
  return(result)
}
