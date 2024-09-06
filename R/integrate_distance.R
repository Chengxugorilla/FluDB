integrate_distance <- function(D_list,GISAID_ref){
  ref_Virus <- unique(unlist(
    lapply(seq_along(D_list),function(i){
      names(D_list[[i]])
    })))

  result_list <- rep(list(NULL), length(ref_Virus))
  names(result_list) <- ref_Virus
  for(i in seq_along(D_list)){
    my_list <- D_list[[i]]
    for(j in seq_along(my_list)){
      Ref_V <- names(my_list)[j]
      result_list[[Ref_V]] <- c(result_list[[Ref_V]],my_list[[j]])
    }
  }

  ## remove replicate
  result_list2 <-
  lapply(seq_along(result_list), function(i){
    titers <- result_list[[i]]
    titers <- titers[!is.na(titers)]

    test_freq <- table(names(titers))
    multi_ti <- names(test_freq[test_freq > 1])
    if(length(multi_ti) == 0)
      return(titers)

    multi_value <- unlist(
    lapply(seq_along(multi_ti),function(i){
      test_V <- multi_ti[i]
      mean(titers[names(titers) == test_V],na.rm=TRUE)
    }))
    names(multi_value) <- multi_ti

    titers <- c(titers[!names(titers) %in% multi_ti],multi_value)
    if(any(is.nan(titers)))
      stop("fuck!")
    return(titers)
  })

  names(result_list2) <- names(result_list)
  return(result_list2)
}
