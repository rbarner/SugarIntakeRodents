rm(list=ls())
library(ggplot2)
library(ggrepel)
library(dplyr)

setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/RodentSugarIntake/statisticalModels")

#####################################################
taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("..//statisticalModels//")
  pValuesTable <- read.delim(paste("3_",taxa,"_pValueTable_SugarVsControl.txt",sep=""))
  
  pValuesTable$SugarVsControl_BW_c <- -log10(pValuesTable$SugarVsControl_BW)
  pValuesTable$SugarVsControl_BW_c  <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_BW_c,-1*pValuesTable$SugarVsControl_BW_c)
  
  pValuesTable$SugarVsControl_EnergyIntake_c  <- -log10(pValuesTable$SugarVsControl_EnergyIntake)
  pValuesTable$SugarVsControl_EnergyIntake_c  <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_EnergyIntake_c,-1*pValuesTable$SugarVsControl_EnergyIntake_c)
  
  pValuesTable$SugarVsControl_c <- -log10(pValuesTable$SugarVsControl)
  pValuesTable$SugarVsControl_c <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_c,-1*pValuesTable$SugarVsControl_c)
  
  pValuesTable$WithinSugar_c <- -log10(pValuesTable$WithinSugar)
  pValuesTable$WithinSugar_c <- ifelse(pValuesTable$tValueSugar > 0,pValuesTable$WithinSugar_c,-1*pValuesTable$WithinSugar_c)
  
  pValuesTable$BodyWeight_c <- -log10(pValuesTable$BodyWeight)
  pValuesTable$BodyWeight_c  <- ifelse(pValuesTable$BodyWeightSugarEstimate > 0,pValuesTable$BodyWeight_c,-1*pValuesTable$BodyWeight_c)
  
  pValuesTable$EnergyIntake_c  <- -log10(pValuesTable$EnergyIntake)
  pValuesTable$EnergyIntake_c <- ifelse(pValuesTable$EnergyIntakeSugarEstimate > 0,pValuesTable$EnergyIntake_c,-1*pValuesTable$EnergyIntake_c)
  
  pValuesTable$BodyWeightInteraction_c <- -log10(pValuesTable$BodyWeightInteraction)
  pValuesTable$BodyWeightInteraction_c <- ifelse(pValuesTable$BodyWeightSugarInteractionEstimate > 0,pValuesTable$BodyWeightInteraction_c,-1*pValuesTable$BodyWeightInteraction_c)
  
  pValuesTable$EnergyIntakeInteraction_c  <- -log10(pValuesTable$EnergyIntakeInteraction)
  pValuesTable$EnergyIntakeInteraction_c <- ifelse(pValuesTable$EnergyIntakeSugarInteractionEstimate > 0,pValuesTable$EnergyIntakeInteraction_c,-1*pValuesTable$EnergyIntakeInteraction_c)
  
  setwd("..//plots//")
  p <- ggplot(pValuesTable)
  
  tiff(paste("6_",taxa,"_sugarVsControlAgainstBodyWeightInteraction.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_BW_c,BodyWeightInteraction_c),
                 colour = ifelse(pValuesTable$SugarVsControl_BW_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$BodyWeightInteraction_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_BW_Adj < 0.1,5,
                               ifelse(pValuesTable$BodyWeightInteraction_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight Interaction with Sugar Intake p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_BW_Adj < 0.1 | BodyWeightInteraction_Adj < 0.1),
                    aes(SugarVsControl_BW_c,BodyWeightInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
    )
  )
  graphics.off()
  
  tiff(paste("6_",taxa,"_sugarVsControlAgainstBodyWeight.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_BW_c,BodyWeight_c),
                 colour = ifelse(pValuesTable$SugarVsControl_BW_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$BodyWeight_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_BW_Adj < 0.1,5,
                               ifelse(pValuesTable$BodyWeight_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_BW_Adj < 0.1 | BodyWeight_Adj < 0.1),
                    aes(SugarVsControl_BW_c,BodyWeight_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
    )
  )
  graphics.off()
  
  tiff(paste("6_",taxa,"_sugarVsControlAgainstBodyWeightInteraction.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_BW_c,BodyWeightInteraction_c),
                 colour = ifelse(pValuesTable$SugarVsControl_BW_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$BodyWeightInteraction_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_BW_Adj < 0.1,5,
                               ifelse(pValuesTable$BodyWeightInteraction_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight Interaction with Sugar Intake p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_BW_Adj < 0.1 | BodyWeightInteraction_Adj < 0.1),
                    aes(SugarVsControl_BW_c,BodyWeightInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
    )
  )
  graphics.off()
  
  tiff(paste("6_",taxa,"_sugarVsControlAgainstEnergyIntake.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_EnergyIntake_c,EnergyIntake_c),
                 colour = ifelse(pValuesTable$SugarVsControl_EnergyIntake_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$EnergyIntake_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_EnergyIntake_Adj < 0.1,5,
                               ifelse(pValuesTable$EnergyIntake_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_EnergyIntake_Adj < 0.1 | EnergyIntake_Adj < 0.1),
                    aes(SugarVsControl_EnergyIntake_c,EnergyIntake_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
    )
  )
  graphics.off()
  
  
  tiff(paste("6_",taxa,"_sugarVsControlAgainstEnergyIntakeInteraction.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_EnergyIntake_c,EnergyIntakeInteraction_c),
                 colour = ifelse(pValuesTable$SugarVsControl_EnergyIntake_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$EnergyIntakeInteraction_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_EnergyIntake_Adj < 0.1,5,
                               ifelse(pValuesTable$EnergyIntakeInteraction_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake Interaction with Sugar Intake p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_EnergyIntake_Adj < 0.1 | EnergyIntakeInteraction_Adj < 0.1),
                    aes(SugarVsControl_EnergyIntake_c,EnergyIntakeInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
         )
    )
  graphics.off()
  
  tiff(paste("6_",taxa,"_sugarVsControlAgainstWithinSugar.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_c,WithinSugar_c),
                 colour = ifelse(pValuesTable$SugarVsControl_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$WithinSugar_Adj < 0.12,"blue3","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_Adj < 0.1,5,
                               ifelse(pValuesTable$WithinSugar_Adj < 0.12,5,3))) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Fructose:Glucose ratio p-value)") +
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_Adj < 0.1 | WithinSugar_Adj < 0.12),
                    aes(SugarVsControl_c,WithinSugar_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
          )
  )
  graphics.off()
}