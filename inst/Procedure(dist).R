H3N2_list1 <- rm_redundant_col(H3N2_list)
H3N2_list2 <- titer_standardization(H3N2_list1)
H3_distance <- antigenic_Distance(H3N2_list2)
H3_dist_all <- integrate_distance(H3_distance)
H3_df <- data4model(H3_dist_all,GISAID_H3N2,"H3")

H1N1_list1 <- rm_redundant_col(H1N1_list)
H1N1_list2 <- titer_standardization(H1N1_list1)
H1_distance <- antigenic_Distance(H1N1_list2)
H1_dist_all <- integrate_distance(H1_distance)
H1_df <- data4model(H1_dist_all,GISAID_H1N1,"H1")

final_df <- rbind(H3_df,H1_df)
sel_row <-
apply(final_df,1,function(x){
  if(any(x == ""))
    return(FALSE)
  else
    return(TRUE)
})
final_df <- final_df[sel_row,]
write.csv(final_df,file = "C:/Users/19177/Desktop/data4model(dist).csv",row.names = FALSE)

seq_names <- c(final_df$reference_id,final_df$candidate_id)
seq_content <- c(final_df$reference_seq,final_df$candidate_seq)
df <- data.frame(seq_names,seq_content)

unique_rows <- !duplicated(df$seq_names)
df <- df[unique_rows,]

seqs_fasta <- c()
for (i in seq_along(df[,1])) {
  seqs_fasta <- c(seqs_fasta,paste0(">",df[i,1]),df[i,2])
}

writeLines(seqs_fasta,con = "C:/Users/19177/Desktop/Dist.fasta")
ref_fasta <- read_sequences("C:/Users/19177/Desktop/Dist_alignment.fasta","AA")

final_df_alignment <- t(
apply(final_df, 1, function(x){
  ref <- x[1]
  ref_idx <- which(ref_fasta[[1]] == ref)
  x[2] <- ref_fasta[[2]][ref_idx]
  test <- x[3]
  test_idx <- which(ref_fasta[[1]] == test)
  x[4] <- ref_fasta[[2]][test_idx]
  return(x)
}))
write.csv(final_df_alignment,file = "C:/Users/19177/Desktop/data4model(alignment).csv",
          row.names = FALSE)
final_df$candidate_id
distance_list[[241]]
which(unlist(
lapply(seq_along(distance_list_total), function(i){
  any(is.nan(unlist(distance_list_total[[i]])))
  #any(unlist(distance_list[[i]])=="NaN")
  #any(!is.numeric(unlist(distance_list_total[[i]])))
  #is.na(distance_list[[i]])
})))


