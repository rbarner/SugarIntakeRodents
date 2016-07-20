rm(list=ls())
library("vegan")

################# RDP tables ##################################
taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/data/")
  inFileName <- paste("rdp_",taxa,"_counts.txt",sep="");
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  numCols <- ncol(myT)
  myColClasses <- c("character", rep("numeric", numCols-1))
  myT <-read.table(inFileName,header=TRUE,sep="\t",row.names=1,colClasses=myColClasses)
  myTLogged <- log10((myT/rowSums(myT))*(sum(rowSums(myT))/dim(myT)[1]) +1)
  myPCOA <- capscale(myTLogged~1,distance="bray")
  
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/mds/")
  eigenNames=cbind("X","loading")
  write.table(myPCOA$CA$u, sep="\t", file=paste("mds_", taxa, "_rdp.txt",sep=""))
  write.table(eigenNames,file=paste("eigenValues_", taxa, "_rdp.txt", sep=""), sep="\t",col.names=FALSE,row.names=FALSE)
  write.table(myPCOA$CA$eig/sum(myPCOA$CA$eig),file=paste("eigenValues_", taxa, "_rdp.txt", sep=""), sep="\t",col.names=FALSE,append=TRUE)
}
######################################## QIIME tables #################################

taxaLevels <- c("phylum","class","order","family","genus","otuNoTaxonomy")
for(taxa in taxaLevels )
{
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/data/")
  inFileName <- paste("qiime_",taxa,"_counts.txt",sep="");
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  numCols <- ncol(myT)
  myColClasses <- c("character", rep("numeric", numCols-1))
  myT <-read.table(inFileName,header=TRUE,sep="\t",row.names=1,colClasses=myColClasses)
  myT <- t(myT)
  myTLogged <- log10((myT/rowSums(myT))*(sum(rowSums(myT))/dim(myT)[1]) +1)
  myPCOA <- capscale(myTLogged~1,distance="bray")
  
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/mds/")
  eigenNames=cbind("X","loading")
  write.table(myPCOA$CA$u, sep="\t", file=paste("mds_", taxa, "_qiime.txt",sep=""))
  write.table(eigenNames,file=paste("eigenValues_", taxa, "_qiime.txt", sep=""), sep="\t",col.names=FALSE,row.names=FALSE)
  write.table(myPCOA$CA$eig/sum(myPCOA$CA$eig),file=paste("eigenValues_", taxa, "_qiime.txt", sep=""), sep="\t",col.names=FALSE,append=TRUE)
}