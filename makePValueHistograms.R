################################################################### Make Plots ################################
rm(list=ls())
library(ggplot2)

taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//statisticalModels//")
  pValuesTable <- read.delim(paste("pValueTable_SugarVsControl_",taxa,"_rdp.txt",sep=""))
  
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//plots//")
  p <- ggplot(pValuesTable)
  
  tiff(paste("BodyWeightInteraction_histogram_",taxa,"_rdp.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(BodyWeightInteraction),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Body weight Interaction p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("BodyWeight_histogram_",taxa,"_rdp.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(BodyWeight),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Body weight p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("EnergyIntake_histogram_",taxa,"_rdp.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(FoodIntake),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Energy Intake p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  
  tiff(paste("EnergyIntakeInteraction_histogram_",taxa,"_rdp.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(FoodIntakeInteraction),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Energy Intake Interaction p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("sugarVsControl_histogram_",taxa,"_rdp.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(SugarVsControl),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Sugar Vs Control p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()

  tiff(paste("withinSugar_histogram_",taxa,"_rdp.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(WithinSugar),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Fructose:Glucose group p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
}
##################################################### QIIME plots ####################################################
taxaLevels <- c("phylum","class","order","family","genus","otuNoTaxonomy")
for(taxa in taxaLevels )
{
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//statisticalModels//")
  pValuesTable <- read.delim(paste("pValueTable_SugarVsControl_",taxa,"_qiime.txt",sep=""))

  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//plots//")
  p <- ggplot(pValuesTable)
  
  tiff(paste("BodyWeightInteraction_histogram_",taxa,"_qiime.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(BodyWeightInteraction),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Body weight Interaction p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("BodyWeight_histogram_",taxa,"_qiime.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(BodyWeight),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Body weight p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("EnergyIntake_histogram_",taxa,"_qiime.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(FoodIntake),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Energy Intake p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  
  tiff(paste("EnergyIntakeInteraction_histogram_",taxa,"_qiime.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(FoodIntakeInteraction),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Energy Intake Interaction p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("sugarVsControl_histogram_",taxa,"_qiime.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(SugarVsControl),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Sugar Vs Control p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  tiff(paste("withinSugar_histogram_",taxa,"_qiime.tiff",sep=""),width=3200,height=3200,compression="lzw",res=300);
  print(p + geom_histogram(aes(WithinSugar),fill="grey76",colour="black",binwidth=0.1) +
          xlab("P-values")+ ylab("Counts") +
          guides(colour=FALSE)+
          ggtitle("Fructose:Glucose group p-values")+
          theme_classic(base_size = 34)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=34)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
}