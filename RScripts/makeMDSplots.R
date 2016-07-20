rm(list=ls())
library(ggplot2)

sampleData = read.delim("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/metaData/metadata.txt")
intakeData <- read.delim("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/metaData/recentIntakeData.txt")
noic  <- read.delim("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/metaData/NOICdata.txt")

sampleData$sugarIntake  <- intakeData$SolutionIntake
sampleData$foodIntake  <- intakeData$FoodIntake
sampleData$bodyweight  <- intakeData$RatWeight
sampleData$brain  <- noic$NOIC.2min.discrimination.index
sampleData$SugarVsControl  <- ifelse(sampleData$SugarVsControl %in% "Control","Water","Sugar")
sampleData$group  <- ifelse(sampleData$group=='Group_1_(35F:65G)','35F:65G',
                            ifelse(sampleData$group=='Group_4_(50F:50G)','50F:50G',
                                   ifelse(sampleData$group=='Group_2_(65F:35G)','65F:35G','Control')
                            )
)

taxaLevels <- c("phylum","class","order","family","genus")
for(taxa in taxaLevels )
{
  setwd("../mds//")
  mdsFile <- paste(  "mds_", taxa, "_rdp.txt",sep="");
  eigenFile <- paste(  "eigenValues_", taxa, "_rdp.txt",sep="");
  
  mds <-read.table(mdsFile,header=TRUE,sep="\t",row.names=1);
  mdsMeta <- cbind(sampleData,mds);
  eigen <-read.table(eigenFile,header=TRUE,sep="\t");
  
  mdsMetaSugar <- split(mdsMeta,mdsMeta$SugarVsControl)$Sugar
  
  ################ Generate p-values ##############  
  ttest_sugarVsControl_mds1 <- t.test(MDS1~SugarVsControl,mdsMeta);
  pVal_sugarVsControl_mds1 <- ttest_sugarVsControl_mds1$p.value[[1]];
  ttest_sugarVsControl_mds2 <- t.test(MDS2~SugarVsControl,mdsMeta);
  pVal_sugarVsControl_mds2 <- ttest_sugarVsControl_mds2$p.value[[1]];
  
  
  title <- paste("MDS plot (",taxa," level)",sep="")
  comp1<-as.character(paste("MDS1"," ",(round(eigen$loading[1],2))*100,"%, p-val = ",format.pval(pVal_sugarVsControl_mds1,2),sep=""));
  comp2<-as.character(paste("MDS2"," ",(round(eigen$loading[2],2))*100,"%, p-val = ",format.pval(pVal_sugarVsControl_mds2,2),sep=""));
  
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/plots")
  
  tiff(paste("mdsPlot_Axes12_",taxa,"coloredBySugarVsControl_rdp.tiff",sep=""),width=200,height=200,units="mm",compression="lzw",res=350)
  p <- ggplot(mdsMeta)
  print(p + geom_point(aes(MDS1,MDS2,colour = as.factor(SugarVsControl)),size = 10) +
          scale_colour_manual(values=c("red2","blue3")) +
          guides(colour=FALSE)+
          xlab(comp1) + ylab(comp2) +
          theme_classic(base_size = 36)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=36)
          )+
        theme(axis.line.x = element_line(color="black", size = 2),
              axis.line.y = element_line(color="black", size = 2)
              )
  )
  graphics.off()
  
  anova_sugarGroup_mds1 <- lm(MDS1~group,mdsMetaSugar);
  pVal_sugarGroup_mds1 <- anova(anova_sugarGroup_mds1)$"Pr(>F)";
  anova_sugarGroup_mds2 <- lm(MDS2~group,mdsMetaSugar);
  pVal_sugarGroup_mds2 <- anova(anova_sugarGroup_mds2)$"Pr(>F)";
  
  title <- paste("MDS plot (",taxa," level)",sep="")
  comp1<-as.character(paste("MDS1"," ",(round(eigen$loading[1],2))*100,"%, p-val = ",format.pval(pVal_sugarGroup_mds1,2),sep=""));
  comp2<-as.character(paste("MDS2"," ",(round(eigen$loading[2],2))*100,"%, p-val = ",format.pval(pVal_sugarGroup_mds2,2),sep=""));
  
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/plots")
  
  p <- ggplot(mdsMetaSugar)
  tiff(paste("mdsPlot_Axes12_",taxa,"_coloredBySugarGroup_rdp.tiff",sep=""),width=200,height=200,units="mm",compression="lzw",res=350)
  print(p + geom_point(aes(MDS1,MDS2,colour = as.factor(group)),size = 10) +
          scale_colour_manual(values=c("red3","cyan3","darkmagenta")) +
          guides(colour=FALSE)+
          xlab(comp1) + ylab(comp2) +
          theme_classic(base_size = 36)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=36)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
}


##################################### Qiime data #################################333
taxaLevels <- c("phylum","class","order","family","genus","otuNoTaxonomy")
for(taxa in taxaLevels )
{
  setwd("../mds//")
  mdsFile <- paste(  "mds_", taxa, "_qiime.txt",sep="");
  eigenFile <- paste(  "eigenValues_", taxa, "_qiime.txt",sep="");
  
  mds <-read.table(mdsFile,header=TRUE,sep="\t",row.names=1);
  mdsMeta <- cbind(sampleData,mds);
  eigen <-read.table(eigenFile,header=TRUE,sep="\t");
  
  mdsMetaSugar <- split(mdsMeta,mdsMeta$SugarVsControl)$Sugar
  
  ################ Generate p-values ##############  
  ttest_sugarVsControl_mds1 <- t.test(MDS1~SugarVsControl,mdsMeta);
  pVal_sugarVsControl_mds1 <- ttest_sugarVsControl_mds1$p.value[[1]];
  ttest_sugarVsControl_mds2 <- t.test(MDS2~SugarVsControl,mdsMeta);
  pVal_sugarVsControl_mds2 <- ttest_sugarVsControl_mds2$p.value[[1]];
  
  
  title <- paste("MDS plot (",taxa," level)",sep="")
  comp1<-as.character(paste("MDS1"," ",(round(eigen$loading[1],2))*100,"%, p-val = ",format.pval(pVal_sugarVsControl_mds1,2),sep=""));
  comp2<-as.character(paste("MDS2"," ",(round(eigen$loading[2],2))*100,"%, p-val = ",format.pval(pVal_sugarVsControl_mds2,2),sep=""));
  
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/plots")
  
  tiff(paste("mdsPlot_Axes12_",taxa,"coloredBySugarVsControl_qiime.tiff",sep=""),width=200,height=200,units="mm",compression="lzw",res=350)
  p <- ggplot(mdsMeta)
  print(p + geom_point(aes(MDS1,MDS2,colour = as.factor(SugarVsControl)),size = 10) +
          scale_colour_manual(values=c("red2","blue3")) +
          xlab(comp1) + ylab(comp2) +
          guides(colour=FALSE)+
          theme_classic(base_size = 36)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=36)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
  
  anova_sugarGroup_mds1 <- lm(MDS1~group,mdsMetaSugar);
  pVal_sugarGroup_mds1 <- anova(anova_sugarGroup_mds1)$"Pr(>F)";
  anova_sugarGroup_mds2 <- lm(MDS2~group,mdsMetaSugar);
  pVal_sugarGroup_mds2 <- anova(anova_sugarGroup_mds2)$"Pr(>F)";
  
  title <- paste("MDS plot (",taxa," level)",sep="")
  comp1<-as.character(paste("MDS1"," ",(round(eigen$loading[1],2))*100,"%, p-val = ",format.pval(pVal_sugarGroup_mds1,2),sep=""));
  comp2<-as.character(paste("MDS2"," ",(round(eigen$loading[2],2))*100,"%, p-val = ",format.pval(pVal_sugarGroup_mds2,2),sep=""));
  
  setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/paper/plots")
  
  p <- ggplot(mdsMetaSugar)
  tiff(paste("mdsPlot_Axes12_",taxa,"_coloredBySugarGroup_qiime.tiff",sep=""),width=200,height=200,units="mm",compression="lzw",res=350)
  print(p + geom_point(aes(MDS1,MDS2,colour = as.factor(group)),size = 10) +
          scale_colour_manual(values=c("red3","cyan3","darkmagenta")) +
          xlab(comp1) + ylab(comp2) +
          guides(colour=FALSE)+
          theme_classic(base_size = 36)+
          theme(text=element_text(face="bold",size=24),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=24),
                axis.title=element_text(face="bold",size=36)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
  graphics.off()
}