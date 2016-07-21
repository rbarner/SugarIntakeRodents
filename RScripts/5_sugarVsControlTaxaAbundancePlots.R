rm(list=ls())
library(calibrate)
library(ggplot2)
library(ggrepel)
library(dplyr)

setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/RodentSugarIntake/data")
setwd(paste("../metaData",sep=""))
metaData <- read.delim("metadata.txt")

#############################################
taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("../data/")
  if(taxa %in% "otuNoTaxonomy")
  {
    abundanceFileName <- paste("qiime_",taxa,"_counts.txt",sep="");
  }
  else
  {
    abundanceFileName <- paste("rdp_",taxa,"_classified.txt",sep="");
  }
  abundance <-read.delim(abundanceFileName,header=TRUE,row.names=1);
  abundance <- t(abundance);
  
  #Log normalize taxa abundances
  abundance <- log10((abundance/rowSums(abundance))*(sum(rowSums(abundance))/dim(abundance)[1]) +1);
  
  #Filter out taxa that are only present in less than  25% of the samples
  abundance<- abundance[,(colSums(abundance==0)/dim(abundance)[1])<0.25]
  colnames(abundance) <- gsub("-","_",colnames(abundance))
  colnames(abundance) <- gsub(" ","_",colnames(abundance))
  colnames(abundance) <- gsub("/","_",colnames(abundance))
  colnames(abundance) <- gsub("\\[","",colnames(abundance))
  colnames(abundance) <- gsub("\\]","",colnames(abundance))
  abundance <- cbind(abundance,metaData);
  #abundance <- as.data.frame(abundance);
  abundance$group <- factor(abundance$group,levels = c("Group_3_(control)","Group_1_(35F:65G)","Group_4_(50F:50G)","Group_2_(65F:35G)"));
  
  sugarAbundance  <- split(abundance,abundance$SugarVsControl)$Sugar
  controlAbundance  <- split(abundance,abundance$SugarVsControl)$Control
  sugarAbundance$colors  <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","blue",ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)","purple","red"))
  sugarAbundance$sugarGroups  <- ifelse(sugarAbundance$group %in% "Group_1_(35F:65G)","35% Fruct",ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","65% Fruct","50% Fruct"))
  sugarAbundance$groupNum <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)",65,ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)",50,35))
     
  sugarAbundanceNoMeta <- sugarAbundance[,1:(dim(sugarAbundance)[2]-4)]
  sugarAbundanceNoMeta[,1:dim(sugarAbundanceNoMeta)[2]] <- sapply(sugarAbundanceNoMeta[,1:dim(sugarAbundanceNoMeta)[2]], as.numeric)

  controlAbundanceNoMeta   <- controlAbundance[,1:(dim(sugarAbundance)[2]-4)]
  controlAbundanceNoMeta[,1:dim(controlAbundanceNoMeta)[2]] <- sapply(controlAbundanceNoMeta[,1:dim(controlAbundanceNoMeta)[2]], as.numeric)

  pValsTable <- read.delim(paste("../statisticalModels/3_",taxa,"_pValueTable_SugarVsControl.txt",sep=""),row.names=1)

  sugarAbundanceNoMeta <- sugarAbundanceNoMeta[,names(sugarAbundanceNoMeta) %in% row.names(pValsTable) ]
  controlAbundanceNoMeta <- controlAbundanceNoMeta[,names(controlAbundanceNoMeta) %in% row.names(pValsTable) ]

  controlMeans <- colMeans(controlAbundanceNoMeta)
  sugarMeans <- colMeans(sugarAbundanceNoMeta)

  meanTable <- data.frame(controlMeans,sugarMeans)
  
  setwd("..//plots//")

  meanTable <- data.frame(name=names(controlMeans),controlMeans,sugarMeans,pVals=pValsTable$SugarVsControl_Adj)
  p <- ggplot(meanTable)
  tiff(paste("5_",taxa,"_abundancePlot_coloredBySugarVsControlPVal.tiff",sep=""),width=4200,height=4200,compression="lzw",res=350)
  print(p + geom_point(aes(controlMeans,sugarMeans),
                colour = ifelse(meanTable$pVals>0.1 | (controlMeans < 0.5 & sugarMeans < 0.5),
                                 'gray53','red2'),
                size = 6) +
    xlab("Log10 (water group), n=10")+ ylab("Log10 (all sugar groups), n=32") +
    geom_abline(slope = 1,intercept = 0)+
    geom_text_repel(data = filter(meanTable,!(pVals>0.1 | (controlMeans < 0.5 & sugarMeans < 0.5))),
                   aes(controlMeans,sugarMeans,
                        label = name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
          )
  )
  graphics.off()
}