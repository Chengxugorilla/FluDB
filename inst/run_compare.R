seqs <- read_sequences("C:/Users/19177/Desktop/FluDB/inst/extdata/H1N1.fasta")
seqs[[2]][1]

library(xml2)
library(readxl)
"C:\Users\19177\Desktop\HI.pzfx"
file_path <- "C:/Users/19177/Desktop/HI.pzfx"
pzfx_data <- read_xml(file_path)

# 打印文件内容
print(pzfx_data)

library(magrittr)
VIRUS <- c('A/Hunan/42443/2015','A/California/04/2009','A/California/7/2009',
           'A/Michigan/45/2015','A/Brisbane/02/2018','A/Victoria/2570/2019','A/Victoria/4897/2022') %>%
  factor(rep(.,2),levels = .)
HI <-
  c(533.2721202,1770.484005,2081.673505,1471.965451,1240.175084,327.7218593,
    20.84931522,112.3244428,48.05877916,34.39979578,54.50543141,29.86384653,
    1227.177392,1280)
GROUP <- rep(c("OURS","WHO"),each=7)
TB <- data.frame(VIRUS,HI,GROUP)

library(ggplot2)
ggplot(TB, aes(x = VIRUS, y = HI, fill = GROUP)) +
  geom_bar(stat = "identity", position = position_dodge(1)) +
  theme_minimal()
