################################################################### Make Plots ################################
rm(list=ls())
library(ggplot2)
library(ggrepel)
library(dplyr)

taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//statisticalModels//")
  pValuesTable <- read.delim(paste("pValueTable_SugarVsControl_",taxa,"_rdp.txt",sep=""))
  
  pValuesTable$SugarVsControl_BW_c <- -log10(pValuesTable$SugarVsControl_BW)
  pValuesTable$SugarVsControl_BW_c  <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_BW_c,-1*pValuesTable$SugarVsControl_BW_c)
  
  pValuesTable$SugarVsControl_FoodIntake_c  <- -log10(pValuesTable$SugarVsControl_FoodIntake)
  pValuesTable$SugarVsControl_FoodIntake_c  <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_FoodIntake_c,-1*pValuesTable$SugarVsControl_FoodIntake_c)
  
  pValuesTable$SugarVsControl_c <- -log10(pValuesTable$SugarVsControl)
  pValuesTable$SugarVsControl_c <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_c,-1*pValuesTable$SugarVsControl_c)
  
  pValuesTable$WithinSugar_c <- -log10(pValuesTable$WithinSugar)
  pValuesTable$WithinSugar_c <- ifelse(pValuesTable$tValueSugar > 0,pValuesTable$WithinSugar_c,-1*pValuesTable$WithinSugar_c)
  
  pValuesTable$BodyWeight_c <- -log10(pValuesTable$BodyWeight)
  pValuesTable$BodyWeight_c  <- ifelse(pValuesTable$BodyWeightSugarEstimate > 0,pValuesTable$BodyWeight_c,-1*pValuesTable$BodyWeight_c)
  
  pValuesTable$FoodIntake_c  <- -log10(pValuesTable$FoodIntake)
  pValuesTable$FoodIntake_c <- ifelse(pValuesTable$FoodIntakeSugarEstimate > 0,pValuesTable$FoodIntake_c,-1*pValuesTable$FoodIntake_c)
  
  pValuesTable$BodyWeightInteraction_c <- -log10(pValuesTable$BodyWeightInteraction)
  pValuesTable$BodyWeightInteraction_c <- ifelse(pValuesTable$BodyWeightSugarInteractionEstimate > 0,pValuesTable$BodyWeightInteraction_c,-1*pValuesTable$BodyWeightInteraction_c)
  
  pValuesTable$FoodIntakeInteraction_c  <- -log10(pValuesTable$FoodIntakeInteraction)
  pValuesTable$FoodIntakeInteraction_c <- ifelse(pValuesTable$FoodIntakeSugarInteractionEstimate > 0,pValuesTable$FoodIntakeInteraction_c,-1*pValuesTable$FoodIntakeInteraction_c)
  
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//plots//")
  p <- ggplot(pValuesTable)
  
  tiff(paste("sugarVsControlAgainstBodyWeightInteraction_",taxa,"_rdp.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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
  
  tiff(paste("sugarVsControlAgainstBodyWeight_",taxa,"_rdp.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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
  
  tiff(paste("sugarVsControlAgainstBodyWeightInteraction_",taxa,"_rdp.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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
  
  tiff(paste("sugarVsControlAgainstFoodIntake_",taxa,"_rdp.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_FoodIntake_c,FoodIntake_c),
                 colour = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$FoodIntake_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,5,
                               ifelse(pValuesTable$FoodIntake_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_FoodIntake_Adj < 0.1 | FoodIntake_Adj < 0.1),
                    aes(SugarVsControl_FoodIntake_c,FoodIntake_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
    )
  )
  graphics.off()
  
  
  tiff(paste("sugarVsControlAgainstFoodIntakeInteraction_",taxa,"_rdp.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_FoodIntake_c,FoodIntakeInteraction_c),
                 colour = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,"red2",
                                 ifelse(pValuesTable$FoodIntakeInteraction_Adj < 0.1,"red2","gray53")),
                 size = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,5,
                               ifelse(pValuesTable$FoodIntakeInteraction_Adj < 0.1,5,3))) +
    xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake Interaction with Sugar Intake p-value)") +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_text_repel(data = filter(pValuesTable,SugarVsControl_FoodIntake_Adj < 0.1 | FoodIntakeInteraction_Adj < 0.1),
                    aes(SugarVsControl_FoodIntake_c,FoodIntakeInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
    theme_bw(base_size = 24)+
    theme(axis.line=element_line(size=1),
          axis.ticks=element_line(size=1),
          axis.text=element_text(face="bold",size=16),
          text=element_text(face="bold",size=24)
         )
    )
  graphics.off()
  
  tiff(paste("sugarVsControlAgainstWithinSugar_",taxa,"_rdp.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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

##################################################### QIIME plots ####################################################
taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//statisticalModels//")
  pValuesTable <- read.delim(paste("pValueTable_SugarVsControl_",taxa,"_qiime.txt",sep=""))
  
  pValuesTable$SugarVsControl_BW_c <- -log10(pValuesTable$SugarVsControl_BW)
  pValuesTable$SugarVsControl_BW_c  <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_BW_c,-1*pValuesTable$SugarVsControl_BW_c)
  
  pValuesTable$SugarVsControl_FoodIntake_c  <- -log10(pValuesTable$SugarVsControl_FoodIntake)
  pValuesTable$SugarVsControl_FoodIntake_c  <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_FoodIntake_c,-1*pValuesTable$SugarVsControl_FoodIntake_c)
  
  pValuesTable$SugarVsControl_c <- -log10(pValuesTable$SugarVsControl)
  pValuesTable$SugarVsControl_c <- ifelse(pValuesTable$tStatistic > 0,pValuesTable$SugarVsControl_c,-1*pValuesTable$SugarVsControl_c)
  
  pValuesTable$WithinSugar_c <- -log10(pValuesTable$WithinSugar)
  pValuesTable$WithinSugar_c <- ifelse(pValuesTable$tValueSugar > 0,pValuesTable$WithinSugar_c,-1*pValuesTable$WithinSugar_c)
  
  pValuesTable$BodyWeight_c <- -log10(pValuesTable$BodyWeight)
  pValuesTable$BodyWeight_c  <- ifelse(pValuesTable$BodyWeightSugarEstimate > 0,pValuesTable$BodyWeight_c,-1*pValuesTable$BodyWeight_c)
  
  pValuesTable$FoodIntake_c  <- -log10(pValuesTable$FoodIntake)
  pValuesTable$FoodIntake_c <- ifelse(pValuesTable$FoodIntakeSugarEstimate > 0,pValuesTable$FoodIntake_c,-1*pValuesTable$FoodIntake_c)
  
  pValuesTable$BodyWeightInteraction_c <- -log10(pValuesTable$BodyWeightInteraction)
  pValuesTable$BodyWeightInteraction_c <- ifelse(pValuesTable$BodyWeightSugarInteractionEstimate > 0,pValuesTable$BodyWeightInteraction_c,-1*pValuesTable$BodyWeightInteraction_c)
  
  pValuesTable$FoodIntakeInteraction_c  <- -log10(pValuesTable$FoodIntakeInteraction)
  pValuesTable$FoodIntakeInteraction_c <- ifelse(pValuesTable$FoodIntakeSugarInteractionEstimate > 0,pValuesTable$FoodIntakeInteraction_c,-1*pValuesTable$FoodIntakeInteraction_c)
  
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//paper//plots//")
  p <- ggplot(pValuesTable)
  
  tiff(paste("sugarVsControlAgainstBodyWeightInteraction_",taxa,"_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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
  
  tiff(paste("sugarVsControlAgainstBodyWeight_",taxa,"_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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
  
  tiff(paste("sugarVsControlAgainstBodyWeightInteraction_",taxa,"_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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
  
  tiff(paste("sugarVsControlAgainstFoodIntake_",taxa,"_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_FoodIntake_c,FoodIntake_c),
                       colour = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,"red2",
                                       ifelse(pValuesTable$FoodIntake_Adj < 0.1,"red2","gray53")),
                       size = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,5,
                                     ifelse(pValuesTable$FoodIntake_Adj < 0.1,5,3))) +
          xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake p-value)") +
          geom_hline(yintercept=0)+
          geom_vline(xintercept=0)+
          geom_text_repel(data = filter(pValuesTable,SugarVsControl_FoodIntake_Adj < 0.1 | FoodIntake_Adj < 0.1),
                          aes(SugarVsControl_FoodIntake_c,FoodIntake_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
          theme_bw(base_size = 24)+
          theme(axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=16),
                text=element_text(face="bold",size=24)
          )
  )
  graphics.off()
  
  
  tiff(paste("sugarVsControlAgainstFoodIntakeInteraction_",taxa,"_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
  print(p + geom_point(aes(SugarVsControl_FoodIntake_c,FoodIntakeInteraction_c),
                       colour = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,"red2",
                                       ifelse(pValuesTable$FoodIntakeInteraction_Adj < 0.1,"red2","gray53")),
                       size = ifelse(pValuesTable$SugarVsControl_FoodIntake_Adj < 0.1,5,
                                     ifelse(pValuesTable$FoodIntakeInteraction_Adj < 0.1,5,3))) +
          xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake Interaction with Sugar Intake p-value)") +
          geom_hline(yintercept=0)+
          geom_vline(xintercept=0)+
          geom_text_repel(data = filter(pValuesTable,SugarVsControl_FoodIntake_Adj < 0.1 | FoodIntakeInteraction_Adj < 0.1),
                          aes(SugarVsControl_FoodIntake_c,FoodIntakeInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
          theme_bw(base_size = 24)+
          theme(axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=16),
                text=element_text(face="bold",size=24)
          )
  )
  graphics.off()
  
  tiff(paste("sugarVsControlAgainstWithinSugar_",taxa,"_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
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