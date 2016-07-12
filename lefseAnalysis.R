library(dplyr)

setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/lefse/")
myTable <- read.delim("lefseResults1005.txt")
myTable$adj.p.value <- p.adjust(myTable$p.value,method="BH")
myTableSig <- filter(myTable,Log.meanDiscriminant.> 3.0, adj.p.value < 0.15 )
write("Name\tLog(meanAll)\tDiscriminitiveFeature\tLog(meanDiscriminant)\tp.value\tadj.p.value","lefse_Pvalues_Table.txt");
write.table(myTable,"lefse_Pvalues_Table.txt",quote=FALSE, sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE);  


setwd("../rdpClassifications/")
myTable2 <- read.delim("goran_RDP_lefseFormatted_classified2.txt",row.names=1)
