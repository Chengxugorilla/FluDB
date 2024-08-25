library(magrittr)
## 1. Extract and save GISAID information
GISAID_H3N2 <- build_GISAID("C:/Users/19177/Desktop/FluDB/inst/extdata/H3N2.fasta",type='H3N2')
GISAID_H1N1 <- build_GISAID("C:/Users/19177/Desktop/FluDB/inst/extdata/H1N1.fasta",type='H1N1')
write.csv(GISAID_H3N2[[1]],"C:/Users/19177/Desktop/result/H3N2_GISAID.csv")
write.csv(GISAID_H1N1[[1]],"C:/Users/19177/Desktop/result/H1N1_GISAID.csv")

## 2. Read WHO HI table -> remove Test rows -> extract A/..../YYYY part
WHO_H3N2_TB <- read_WHO_xlsx("C:/Users/19177/Desktop/FluDB/inst/extdata/H3N2.xlsx")
WHO_H3N2 <-
  clean_TB(WHO_H3N2_TB) %>%
  clean_Mutation()
WHO_H1N1_TB <- read_WHO_xlsx("C:/Users/19177/Desktop/FluDB/inst/extdata/H1N1 (no H1N2 tag).xlsx")
WHO_H1N1 <-
  clean_TB(WHO_H1N1_TB) %>%
  clean_Mutation()

## 3. Check data regulation
H3N2_1 <- std_Date(WHO_H3N2)
H1N1_1 <- std_Date(WHO_H1N1)
check_TB_Date(H3N2_1)
check_TB_Date(H1N1_1)

H3N2_2 <- std_Colnames(H3N2_1)
H1N1_2 <- std_Colnames(H1N1_1)
check_TB_Colnames(H3N2_2)
check_TB_Colnames(H1N1_2)

## 4. Add Nicknames column
H3N2_3 <- add_Nicknames(H3N2_2,type = "H3N2")
H1N1_3 <- add_Nicknames(H1N1_2,type = "H1N1")

## 5. Turn rows and cols to GISAID_ID
H3N2_4 <- match_Row_Virus(H3N2_3,GISAID_H3N2[[1]],type="H3N2")
H1N1_4 <- match_Row_Virus(H1N1_3,GISAID_H1N1[[1]],type="H1N1")
check_matched(H1N1_4)

H3N2_list <- match_Col_Virus(H3N2_4,GISAID_H3N2[[1]],type="H3N2")
H1N1_list <- match_Col_Virus(H1N1_4,GISAID_H1N1[[1]],type="H1N1")

## 6. Bind table into one table
library(plyr)
H3N2_table <- do.call("rbind.fill",H3N2_list)
H1N1_table <- do.call("rbind.fill",H1N1_list)
H3N2_table1 <- H3N2_table[H3N2_table$GISAID_ID != 0,colnames(H3N2_table) != "Nicknames"]
H1N1_table1 <- H1N1_table[H1N1_table$GISAID_ID != 0,colnames(H1N1_table) != "Nicknames"]
H3N2_table2 <- H3N2_table1[!is.na(H3N2_table1$GISAID_ID),]
H1N1_table2 <- H1N1_table1[!is.na(H1N1_table1$GISAID_ID),]
H3N2_table3 <- H3N2_table2[,-c(2,3,4,5)]
H1N1_table3 <- H1N1_table2[,-c(2,3,4,5)]

## 7. Combine replicated GISAID_ID in rows
H3N2_final <- rm_Replicated(H3N2_table3)
H1N1_final <- rm_Replicated(H1N1_table3)

## 8. Save table
write.csv(H3N2_final,file = "C:/Users/19177/Desktop/result/H3N2-0727.csv",row.names = FALSE)
write.csv(H1N1_final,file = "C:/Users/19177/Desktop/result/H1N1-0727.csv",row.names = FALSE)

## 9. Generate json file
Generate_json(H3N2_final,GISAID_H3N2,"C:/Users/19177/Desktop/result/H3N2")
Generate_json(H1N1_final,GISAID_H1N1,"C:/Users/19177/Desktop/result/H1N1")

