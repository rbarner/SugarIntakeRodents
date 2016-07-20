rm(list=ls())

setwd(paste("C://Users//Roshonda//Dropbox//FodorLab//GoranData/spreadsheets/",sep=""))
bw  <- read.delim("../bwData.txt")
fIntake  <- read.delim("../foodIntakeData.txt")
sIntake  <- read.delim("../sugarIntakeData.txt")
tIntake  <- read.delim("../totalCalorieIntake.txt")
noic  <- read.delim("../NOICdata.txt")

setwd(paste("C://Users//Roshonda//Dropbox//FodorLab//GoranData/spreadsheets/",sep=""))
abundanceFileName <- paste("pivoted_genusasColumnsLogNormalPlusMetadata.txt",sep="");
abundance <-read.delim(abundanceFileName,header=TRUE,row.names=1);
abundance$group <- factor(abundance$group,levels = c("Group_3_(control)","Group_1_(35F:65G)","Group_4_(50F:50G)","Group_2_(65F:35G)"))
    
abundance$SugarVsControl  <- ifelse(abundance$group %in% c("Group_1_(35F:65G)","Group_2_(65F:35G)","Group_4_(50F:50G)"),"Sugar","Control")
abundance$sugarIntake  <- sIntake[,18]
abundance$totalIntake  <- tIntake[,18]
abundance$foodIntake  <- fIntake[,18]
abundance$bodyweight  <- bw[,18]
abundance$brain  <- noic$NOIC.2min.discrimination.index
    
sugarAbundance  <- split(abundance,abundance$SugarVsControl)$Sugar
controlAbundance <- split(abundance,abundance$SugarVsControl)$Control
sugarAbundance$colors  <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","cyan3",ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)","darkmagenta","red"))
sugarAbundance$sugarGroups  <- ifelse(sugarAbundance$group %in% "Group_1_(35F:65G)","35% Fruct",ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","65% Fruct","50% Fruct"))
    
abundance$colors  <- ifelse(abundance$SugarVsControl %in% "Control","blue","red")
    
sugarAbundanceNoMeta <- sugarAbundance[,3:214]
sugarAbundanceNoMeta[,1:dim(sugarAbundanceNoMeta)[2]] <- sapply(sugarAbundanceNoMeta[,1:dim(sugarAbundanceNoMeta)[2]], as.numeric)

controlAbundanceNoMeta   <- controlAbundance[,3:214]
controlAbundanceNoMeta[,1:dim(controlAbundanceNoMeta)[2]] <- sapply(controlAbundanceNoMeta[,1:dim(controlAbundanceNoMeta)[2]], as.numeric)

pValsTable <- read.delim("../statisticalModels/pValueTable_SugarVsControl_genus.txt",row.names=1)
pValsTable <- pValsTable[-c(1,2),]

sugarAbundanceNoMeta <- sugarAbundanceNoMeta[,names(sugarAbundanceNoMeta) %in% row.names(pValsTable) ]
controlAbundanceNoMeta <- controlAbundanceNoMeta[,names(controlAbundanceNoMeta) %in% row.names(pValsTable) ]

controlMeans <- colMeans(controlAbundanceNoMeta)
sugarMeans <- colMeans(sugarAbundanceNoMeta)

meanTable <- data.frame(controlMeans,sugarMeans)


setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//mds//")
op <- par(mar = c(9,10,4,2) + 0.1)
  
setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//plots//")
svg(paste("abundancePlot_genus_coloredBySugarVsControlPVal.svg",sep=""),width=7,height=7)
plot(controlMeans,sugarMeans,
     cex=4,pch=19,col=ifelse(pValsTable$SugarVsControl_Adj>0.1 | (controlMeans < 0.5 & sugarMeans < 0.5),'black', '#9D2235'),
     xlab="",ylab="",main="",las=1,xaxt='n',yaxt='n'); 
abline(a = 0,b=1,cex=3);
textxy(controlMeans,sugarMeans,
                   labs=ifelse(pValsTable$SugarVsControl_Adj>0.1 | (controlMeans < 0.5 & sugarMeans < 0.5),"",row.names(pValsTable)),
                   cex=1)
axis(1,cex.axis = 1.5);
axis(2,cex.axis = 1.5);
title(ylab = "Log10 (all sugar groups), n=32",cex.lab =2,line=2.7);
title(xlab = "Log10 (control group), n=10",cex.lab =2,line=3.5);
box();
par(op)
graphics.off()



meanTable <- data.frame(name=names(controlMeans),controlMeans,sugarMeans,pVals=pValsTable$SugarVsControl_Adj)
p <- ggplot(meanTable)
tiff(paste("abundancePlot_genus_coloredBySugarVsControlPVal_test.tiff",sep=""),width=200,height=200,units="mm",compression="lzw",res=350)
p + geom_point(aes(controlMeans,sugarMeans),
               colour = ifelse(pValsTable$SugarVsControl_Adj>0.1 | (controlMeans < 0.5 & sugarMeans < 0.5),
                               'gray53','#9D2235'),
               size = 6) +
  xlab("Log10 (water group), n=10")+ ylab("Log10 (all sugar groups), n=32") +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = filter(meanTable,!(pVals>0.1 | (controlMeans < 0.5 & sugarMeans < 0.5))),
                  aes(controlMeans,sugarMeans,
                      label = name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
        )
dev.off()

setwd(paste("C://Users//Roshonda//Dropbox//FodorLab//GoranData/spreadsheets/",sep=""))
abundanceFileName <- paste("pivoted_familyasColumnsLogNormalPlusMetadata.txt",sep="");
abundance <-read.delim(abundanceFileName,header=TRUE,row.names=1);
abundance$group <- factor(abundance$group,levels = c("Group_3_(control)","Group_1_(35F:65G)","Group_4_(50F:50G)","Group_2_(65F:35G)"))

abundance$SugarVsControl  <- ifelse(abundance$group %in% c("Group_1_(35F:65G)","Group_2_(65F:35G)","Group_4_(50F:50G)"),"Sugar","Control")
abundance$sugarIntake  <- sIntake[,18]
abundance$totalIntake  <- tIntake[,18]
abundance$foodIntake  <- fIntake[,18]
abundance$bodyweight  <- bw[,18]
abundance$brain  <- noic$NOIC.2min.discrimination.index

test <- lm(abundance$brain~abundance$Clostridiaceae.2)
anova(test)$"Pr(>F)"[1]

setwd(paste("C://Users//Roshonda//Dropbox//FodorLab//GoranData/plots/",sep=""))
p <- ggplot(abundance)
tiff(paste("abundancePlot_clostridiaceae.tiff",sep=""),width=200,height=200,units="mm",compression="lzw",res=350)
p + geom_point(aes(Clostridiaceae.2,brain),
               colour = 'grey40',
               size = 8) +
  xlab("Log10 (Clostridiaceae.2 Abundance)")+ ylab("NOIC Discrimination Index") +
  geom_abline(slope = coef(test)[2],intercept = coef(test)[1],color="black")+
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24),
        line=element_line(size=1)
       )
dev.off()

test <- lm(abundance$brain~abundance$Clostridiaceae.2)
anova(test)$"Pr(>F)"[1]
tiff(paste("clostridiaceaeAgainstBrain_allSamples.tiff",sep=""),width=2400,height=2200,compression="lzw",res=300);
plot(abundance$Clostridiaceae.2,abundance$brain,col=abundance$colors,pch=19,cex=3,cex.axis=1.5,cex.main = 1.5,cex.lab=1.25,ylab="NOIC Discrimination Index",xlab="Log(Clostridiaceae.2 Abundance)",main = paste("Behavioral index against Clostridiaceae.2 (p-value = ",format.pval(anova(test)$"Pr(>F)"[1],3),")",sep=""))
abline(a = coef(test)[1], b = coef(test)[2], col = "black",lwd=3 )
legend("bottomleft", pch = c(1,1), col = c("blue","red"), legend = c("Control","Sugar"))
dev.off()