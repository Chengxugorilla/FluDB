library(magrittr)
path <- "C:/Users/19177/Desktop/Flu_Data/"
## 1. Extract and save GISAID information
GISAID_Victoria <- build_GISAID("C:/Users/19177/Desktop/Flu_Data/Protein/Victoria/All.fasta",
                                type='H3N2',seq_type = "AA")
GISAID_Yamagata <- build_GISAID("C:/Users/19177/Desktop/Flu_Data/Protein/Yamagata/All.fasta",
                                type='H1N1',seq_type = "AA")
write.csv(GISAID_Victoria[[1]],"C:/Users/19177/Desktop/Flu_Data/Victoria_GISAID.csv")
write.csv(GISAID_Yamagata[[1]],"C:/Users/19177/Desktop/Flu_Data/Yamagata_GISAID.csv")

## 2. Read WHO HI table -> remove Test rows -> extract A/..../YYYY part
WHO_Victoria_TB <- read_WHO_xlsx("C:/Users/19177/Desktop/Flu_Data/Assay/Victoria.xlsx")
WHO_Victoria <-
  clean_TB(WHO_Victoria_TB) %>%
  clean_Mutation()
WHO_Yamagata_TB <- read_WHO_xlsx("C:/Users/19177/Desktop/Flu_Data/Assay/Yamagata.xlsx")
WHO_Yamagata <-
  clean_TB(WHO_Yamagata_TB) %>%
  clean_Mutation()

## 3. Check data regulation
Victoria_1 <- std_Date(WHO_Victoria)
Yamagata_1 <- std_Date(WHO_Yamagata)
check_TB_Date(Victoria_1)
check_TB_Date(Yamagata_1)

## 4. Fill absent column names
Victoria_2 <- std_Colnames(Victoria_1)
Yamagata_2 <- std_Colnames(Yamagata_1)
check_TB_Colnames(Victoria_2)
check_TB_Colnames(Yamagata_2)

## 5. Add Nicknames column
Victoria_3 <- add_Nicknames(Victoria_2,type = "H3N2")
Yamagata_3 <- add_Nicknames(Yamagata_2,type = "H1N1")

## 6. Turn rows and cols to GISAID_ID and check which rows in table are not matched.
Victoria_4 <- match_Row_Virus(Victoria_3,GISAID_Victoria[[1]],type="H3N2")
Yamagata_4 <- match_Row_Virus(Yamagata_3,GISAID_Yamagata[[1]],type="H1N1")
check_matched(Victoria_4)
check_matched(Yamagata_4)

## 7. Turn columns to GISAID_Id.
Victoria_list <- match_Col_Virus(Victoria_4,GISAID_Victoria[[1]],type="H3N2")
Yamagata_list <- match_Col_Virus(Yamagata_4,GISAID_Yamagata[[1]],type="H1N1")

## 6. Bind table into one table
library(plyr)
Victoria_table <- do.call("rbind.fill",Victoria_list)
Yamagata_table <- do.call("rbind.fill",Yamagata_list)
Victoria_table1 <- Victoria_table[Victoria_table$GISAID_ID != 0,colnames(Victoria_table) != "Nicknames"]
Yamagata_table1 <- Yamagata_table[Yamagata_table$GISAID_ID != 0,colnames(Yamagata_table) != "Nicknames"]
Victoria_table2 <- Victoria_table1[!is.na(Victoria_table1$GISAID_ID),]
Yamagata_table2 <- Yamagata_table1[!is.na(Yamagata_table1$GISAID_ID),]
Victoria_table3 <- Victoria_table2[,-c(2,3,4,5)]
Yamagata_table3 <- Yamagata_table2[,-c(2,3,4,5)]

## 7. Combine replicated GISAID_ID in rows
Victoria_final <- rm_Replicated(Victoria_table3)
Yamagata_final <- rm_Replicated(Yamagata_table3)

## 8. Save sequence in fasta format
saveFasta(path = "C:/Users/19177/Desktop/B/Victoria.fasta",Victoria_final,GISAID_Victoria)


## 9. Save table
write.csv(Victoria_final,file = "C:/Users/19177/Desktop/Flu_Data/Victoria.csv",row.names = FALSE)
write.csv(Yamagata_final,file = "C:/Users/19177/Desktop/Flu_Data/Yamagata.csv",row.names = FALSE)

## 9. Generate json file
Generate_json(Victoria_final,GISAID_Victoria,"C:/Users/19177/Desktop/Flu_Data/Victoria")
Generate_json(Yamagata_final,GISAID_Yamagata,"C:/Users/19177/Desktop/Flu_Data/Yamagata")

