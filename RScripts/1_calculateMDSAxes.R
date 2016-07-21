rm(list=ls())
library("vegan")
setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/RodentSugarIntake/data")

taxaLevels <- c("phylum","class","order","family","genus","otuNoTaxonomy")
for(taxa in taxaLevels )
{
  setwd("../data/")
  
  if(taxa %in% "otuNoTaxonomy")
  {
    inFileName <- paste("qiime_",taxa,"_counts.txt",sep="");
  }
  else
  {
    inFileName <- paste("rdp_",taxa,"_classified.txt",sep="");  
  }
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  numCols <- ncol(myT)
  myColClasses <- c("character", rep("numeric", numCols-1))
  myT <-read.table(inFileName,header=TRUE,sep="\t",row.names=1,colClasses=myColClasses)
  myT <- t(myT)
  myTLogged <- log10((myT/rowSums(myT))*(sum(rowSums(myT))/dim(myT)[1]) +1)
  myTLogged <- myTLogged[,(colSums(myTLogged==0)/dim(myTLogged)[1])<0.25]
  myPCOA <- capscale(myTLogged~1,distance="bray")
  
  setwd("../mds/")
  eigenNames=cbind("X","loading")
  write.table(myPCOA$CA$u, sep="\t", file=paste("1_", taxa, "_mds.txt",sep=""))
  write.table(eigenNames,file=paste("1_", taxa, "_eigenValues.txt", sep=""), sep="\t",col.names=FALSE,row.names=FALSE)
  write.table(myPCOA$CA$eig/sum(myPCOA$CA$eig),file=paste("1_", taxa, "_eigenValues.txt", sep=""), sep="\t",col.names=FALSE,append=TRUE)
}