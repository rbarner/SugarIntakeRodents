rm(list=ls())

setwd(paste("C://Users//Roshonda//Dropbox//FodorLab//GoranData/",sep=""))
intakeData <- read.delim("recentIntakeData.txt")
noic  <- read.delim("NOICdata.txt")
metaData <- read.delim("metadata.txt")
######################################## Differences Between Sugar and Control ##############################
taxaLevels <- c("phylum","class","order","family","genus")
#taxaLevels  <- c("family")
for(taxa in taxaLevels )
{
  setwd(paste("C://Users//Roshonda//Dropbox//FodorLab//GoranData/",sep=""))
  abundanceFileName <- paste("goran_otu_table_filtered_ordered_",taxa,".txt",sep="");
  abundance <-read.delim(abundanceFileName,header=TRUE,row.names=1);
  abundance <- t(abundance);
  abundance <- log10((abundance/rowSums(abundance))*(sum(rowSums(abundance))/dim(abundance)[1]) +1);
  colnames(abundance) <- gsub("-","_",colnames(abundance))
  colnames(abundance) <- gsub(" ","_",colnames(abundance))
  colnames(abundance) <- gsub("/","_",colnames(abundance))
  colnames(abundance) <- gsub("\\[","",colnames(abundance))
  colnames(abundance) <- gsub("\\]","",colnames(abundance))
  abundance <- cbind(abundance,metaData);
  #abundance <- as.data.frame(abundance);
  abundance$group <- factor(abundance$group,levels = c("Group_3_(control)","Group_1_(35F:65G)","Group_4_(50F:50G)","Group_2_(65F:35G)"));
  
  abundance$SugarVsControl  <- ifelse(abundance$group %in% c("Group_1_(35F:65G)","Group_2_(65F:35G)","Group_4_(50F:50G)"),"Sugar","Control")
  abundance$sugarIntake  <- intakeData$SolutionIntake
  abundance$foodIntake  <- intakeData$FoodIntake
  abundance$bodyweight  <- intakeData$RatWeight
  abundance$brain  <- noic$NOIC.2min.discrimination.index
  
  sugarAbundance  <- split(abundance,abundance$SugarVsControl)$Sugar
  controlAbundance  <- split(abundance,abundance$SugarVsControl)$Control
  sugarAbundance$colors  <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","blue",ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)","purple","red"))
  sugarAbundance$sugarGroups  <- ifelse(sugarAbundance$group %in% "Group_1_(35F:65G)","35% Fruct",ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","65% Fruct","50% Fruct"))
  sugarAbundance$groupNum <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)",65,ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)",50,35))
  
  abundance$colors  <- ifelse(abundance$SugarVsControl %in% "Control","blue3","red2")
  
  setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//statisticalModels//")
  
  pValSugarVsControlList=numeric(0);
  pValWithinSugarList=numeric(0);
  
  for(i in 1:(dim(sugarAbundance)[2]-8))
  {
    sugarAbundance[,i] <- as.numeric(as.character(sugarAbundance[,i]))
  }
  
  for(i in 1:(dim(controlAbundance)[2]-8))
  {
    controlAbundance[,i] <- as.numeric(as.character(controlAbundance[,i]))
  }
  
  
  meanSugarList <- numeric(0);
  meanControlList <- numeric(0);  
  
  pValBodyweightInteractionList=numeric(0);
  pValWithinSugar_BodyweightList = numeric(0);
  pValBodyweightList=numeric(0);
  bodyWeightEstimateList <- numeric(0);
  bodyWeightInteractionEstimateList <- numeric(0);
  
  pValSugarIntakeInteractionList=numeric(0);
  pValWithinSugar_SugarIntakeList = numeric(0);
  pValSugarIntakeList=numeric(0);
  
  pValFoodIntakeInteractionList=numeric(0);
  pValWithinSugar_FoodIntakeList = numeric(0);
  pValFoodIntakeList=numeric(0);
  foodIntakeEstimateList <- numeric(0);
  foodIntakeInteractionEstimateList <- numeric(0);
  
  pValBrainInteractionList=numeric(0);
  pValWithinSugar_BrainList=numeric(0);
  pValBrainList=numeric(0);
  
  foldChangeList <- numeric(0);
  tValueSugarList <- numeric(0);
  rowNames=character(0);
  lastIter=dim(abundance)[2];
  
  #pdf(file = paste("associationPlots_SugarVsControl_",taxa,".pdf",sep=""))
  #par(mfrow = c(2,2))
  for(i in 1:(lastIter-9))
  {
    meanSugar <- mean(sugarAbundance[,i])
    meanSugarList[[length(meanSugarList)+1]]  <-  meanSugar;
    meanControl <- mean(controlAbundance[,i])
    meanControlList[[length(meanControlList)+1]]  <-  meanControl;
    
    model1=as.formula(paste(names(abundance)[i],"~","SugarVsControl"));
    print(model1);
    sugarVsControlMod <- t.test(model1,data=abundance); 
    foldChange <- sugarVsControlMod$statistic
    foldChangeList[[length(foldChangeList)+1]]  <-  foldChange;
    pValSugarVsControl <- sugarVsControlMod$p.value;
    pValSugarVsControlList[[length(pValSugarVsControlList)+1]]  <-  pValSugarVsControl;
    
    model2=as.formula(paste(names(sugarAbundance)[i],"~","groupNum"));
    print(model2);
    withinSugarMod <- lm(model2,data=sugarAbundance);        
    pValWithinSugar <- anova(withinSugarMod)$"Pr(>F)"[1];
    tValue <- summary(withinSugarMod)$coefficients[2,3];
    pValWithinSugarList[[length(pValWithinSugarList)+1]]  <-  pValWithinSugar;
    tValueSugarList[[length(tValueSugarList)+1]] <- tValue
    
    #boxplot(model1,data=abundance, 
    #        main = paste(names(abundance)[i], ", p-value = ", format.pval(pValSugarVsControl,3),sep = ""),
    #        ylab = "Log(Abundance)")
    #points(ifelse(abundance$SugarVsControl %in% "Sugar",2,1), abundance[,i], pch=19, col=ifelse(abundance$SugarVsControl %in% "Sugar","red","blue"),las = 2)
    
    #################################
    
    model3=as.formula(paste(names(abundance)[i],"~","SugarVsControl","*","bodyweight"));
    print(model3);
    withinSugarwBodyweightMod <- lm(model3,data=abundance);        
    pValBodyweightInteraction <- anova(withinSugarwBodyweightMod)$"Pr(>F)"[3];
    pValBodyweight <- anova(withinSugarwBodyweightMod)$"Pr(>F)"[2];
    #ifelse(foldChange > 0,sugarVsControlMod$p.value,-1*sugarVsControlMod$p.value);
    pValWithinSugar_bodyweight  <- anova(withinSugarwBodyweightMod)$"Pr(>F)"[1];
    
    pValBodyweightInteractionList[[length(pValBodyweightInteractionList)+1]]  <-  pValBodyweightInteraction;
    pValWithinSugar_BodyweightList[[length(pValWithinSugar_BodyweightList)+1]]  <-  pValWithinSugar_bodyweight;
    pValBodyweightList[[length(pValBodyweightList)+1]]  <-  pValBodyweight;
    bodyWeightEstimateList[[length(bodyWeightEstimateList)+1]] <- summary(withinSugarwBodyweightMod)$coefficients[3,1]
    bodyWeightInteractionEstimateList[[length(bodyWeightInteractionEstimateList)+1]] <- summary(withinSugarwBodyweightMod)$coefficients[4,1]
    
#     coefs <- coef(withinSugarwBodyweightMod)
#     plot(abundance$bodyweight,abundance[,i], pch = 19, col = abundance$colors, 
#          main = paste(names(abundance)[i], ", p-value = ", format.pval(pValBodyweight,3),sep = ""),
#          sub = paste("Sugar vs Control Interaction p-value = ", format.pval(pValBodyweightInteraction,3),sep = ""),
#          xlab = "Body weight", ylab = "Log(Abundance)")
#     abline(a = coefs[1]+coefs[2], b = coefs[3]+coefs[4], col = "red") #sugar
#     abline(a = coefs[1], b = coefs[3], col = "blue" )#control
#     #legend("bottomright", pch = c(1,1,1), col = c("blue","purple","red"), legend = c("65","50","35"))
#     
    #################################
    
    model4=as.formula(paste(names(abundance)[i],"~","SugarVsControl","*","foodIntake"));
    print(model4);
    withinSugarwFoodIntakeMod <- lm(model4,data=abundance);        
    pValFoodIntakeInteraction <- anova(withinSugarwFoodIntakeMod)$"Pr(>F)"[3];
    pValFoodIntake <- anova(withinSugarwFoodIntakeMod)$"Pr(>F)"[2];
    pValWithinSugar_FoodIntake  <- anova(withinSugarwFoodIntakeMod)$"Pr(>F)"[1];
    
    pValFoodIntakeInteractionList[[length(pValFoodIntakeInteractionList)+1]]  <-  pValFoodIntakeInteraction;
    pValWithinSugar_FoodIntakeList[[length(pValWithinSugar_FoodIntakeList)+1]]  <-  pValWithinSugar_FoodIntake;
    pValFoodIntakeList[[length(pValFoodIntakeList)+1]]  <-  pValFoodIntake;
    foodIntakeEstimateList[[length(foodIntakeEstimateList)+1]] <- summary(withinSugarwFoodIntakeMod)$coefficients[3,1]
    foodIntakeInteractionEstimateList[[length(foodIntakeInteractionEstimateList)+1]] <- summary(withinSugarwFoodIntakeMod)$coefficients[4,1]
    
#     coefs <- coef(withinSugarwFoodIntakeMod)
#     plot(abundance$foodIntake,abundance[,i], pch = 19, col = abundance$colors, 
#          main = paste(names(abundance)[i], ", p-value = ", format.pval(pValFoodIntake,3),sep = ""),
#          sub = paste("Sugar vs Control p-value = ", format.pval(pValFoodIntakeInteraction,3),sep = ""),
#          xlab = "Food Intake (kCal)", ylab = "Log(Abundance)")
#     abline(a = coefs[1]+coefs[2], b = coefs[3]+coefs[4], col = "red") #sugar
#     abline(a = coefs[1], b = coefs[3], col = "blue" )#control
#     
    #legend("bottomright", pch = c(1,1,1), col = c("blue","purple","red"), legend = c("65% Fructose","50% Fructose","35% Fructose"))
    
    
    #################################
    
    model6=as.formula(paste(names(sugarAbundance)[i],"~","SugarVsControl","*","brain"));
    print(model6);
    withinSugarwBrainMod <- lm(model6,data=abundance);        
    pValBrainInteraction <- anova(withinSugarwBrainMod)$"Pr(>F)"[3];
    pValBrain <- anova(withinSugarwBrainMod)$"Pr(>F)"[2];
    pValWithinSugar_Brain  <- anova(withinSugarwBrainMod)$"Pr(>F)"[1];
    
    pValBrainInteractionList[[length(pValBrainInteractionList)+1]]  <-  pValBrainInteraction;
    pValWithinSugar_BrainList[[length(pValWithinSugar_BrainList)+1]]  <-  pValWithinSugar_Brain;
    pValBrainList[[length(pValBrainList)+1]]  <-  pValBrain;
    
#     coefs <- coef(withinSugarwBrainMod)
#     plot(abundance$brain,abundance[,i], pch = 19, col = abundance$colors, 
#          main = paste(names(abundance)[i], ", p-value = ", format.pval(pValBrain,3),sep = ""),
#          sub = paste("Sugar Vs Control p-value = ", format.pval(pValBrainInteraction,3),sep = ""),
#          xlab = "NOIC Discrimination Index", ylab = "Log(Abundance)")
#     abline(a = coefs[1]+coefs[2], b = coefs[3]+coefs[4], col = "red") #sugar
#     abline(a = coefs[1], b = coefs[3], col = "blue" )#control
#     #legend("bottomright", pch = c(1,1,1), col = c("blue","purple","red"), legend = c("65% Fructose","50% Fructose","35% Fructose"))
    
    rowNames[[length(rowNames)+1]] <- names(abundance)[i];
    
  }
  ############################# 
  #dev.off()
  
#   tiff(paste("pVals_hist_SugarVsControl_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValSugarVsControlList, col="gray77",xlab = "P-value",main ="Sugar Vs Control p-values")  # p-value for sugar vs control types
#   dev.off()
#   
#   tiff(paste("pVals_hist_SugarVsControl_bodyweightInteraction_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValBodyweightInteractionList, col="gray77",xlab = "P-value",main =  "Body weight p-values (interaction with sugar vs control)")  # p-value for body weight within sugar types
#   dev.off()
#   
#   tiff(paste("pVals_hist_SugarVsControl_foodIntakeInteraction_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValFoodIntakeInteractionList, col="gray77",xlab = "P-value",main ="Food intake p-values (interaction with sugar vs control)")  # p-value for food intake within sugar types
#   dev.off()
#   
#   tiff(paste("pVals_hist_SugarVsControl_brainInteraction_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValBrainInteractionList, col="gray77",xlab = "P-value",main ="Behavioral performance p-values (interaction with sugar vs control)")  # p-value for brain within sugar types
#   dev.off()
#   
#   tiff(paste("pVals_hist_SugarVsControl_bodyweight_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValBodyweightList, col="gray77",xlab = "P-value",main =  "Body weight p-values (no interaction)") #P-value for food intake
#   dev.off()
#   
#   tiff(paste("pVals_hist_SugarVsControl_foodIntake_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValFoodIntakeList, col="gray77",xlab = "P-value",main =  "Food intake p-values") #P-value for food intake
#   dev.off()
#   
#   tiff(paste("pVals_hist_SugarVsControl_brain_",taxa,".tiff",sep=""),width=2200,height=2200,compression="lzw",res=300)
#   hist( pValBrainList, col="gray77",xlab = "P-value",main =  "Behavioral performance p-values") #P-value for sugar intake
#   dev.off()
  
  pValSugarVsControlAdj  <- p.adjust(pValSugarVsControlList, method = "fdr")
  pValWithinSugarAdj  <- p.adjust(pValWithinSugarList, method = "fdr")
  
  pValWithinSugar_BodyweightAdj  <- p.adjust(pValWithinSugar_BodyweightList, method = "fdr")
  pValBodyweightInteractionAdj  <- p.adjust(pValBodyweightInteractionList, method = "fdr")
  pValBodyweightAdj  <- p.adjust(pValBodyweightList, method = "fdr")
  
  pValWithinSugar_FoodIntakeAdj  <- p.adjust(pValWithinSugar_FoodIntakeList, method = "fdr")
  pValFoodIntakeInteractionAdj  <- p.adjust(pValFoodIntakeInteractionList, method = "fdr")
  pValFoodIntakeAdj  <- p.adjust(pValFoodIntakeList, method = "fdr")
  
  pValWithinSugar_BrainAdj  <- p.adjust(pValWithinSugar_BrainList, method = "fdr")
  pValBrainInteractionAdj  <- p.adjust(pValBrainInteractionList, method = "fdr")
  pValBrainAdj  <- p.adjust(pValBrainList, method = "fdr")
  
  makeTable=data.frame(rowNames,pValSugarVsControlList,meanSugarList,meanControlList,foldChangeList*-1,tValueSugarList,pValWithinSugarList,
                       pValWithinSugar_BodyweightList,pValBodyweightInteractionList,pValBodyweightList,
                       pValWithinSugar_FoodIntakeList,pValFoodIntakeInteractionList,pValFoodIntakeList,
                       pValWithinSugar_BrainList,pValBrainInteractionList,pValBrainList,
                       pValSugarVsControlAdj,pValWithinSugarAdj,
                       pValWithinSugar_BodyweightAdj,pValBodyweightInteractionAdj,pValBodyweightAdj,
                       pValWithinSugar_FoodIntakeAdj,pValFoodIntakeInteractionAdj,pValFoodIntakeAdj,
                       pValWithinSugar_BrainAdj,pValBrainInteractionAdj,pValBrainAdj,
                       bodyWeightEstimateList,bodyWeightInteractionEstimateList,foodIntakeEstimateList,foodIntakeInteractionEstimateList);
  write("Name\tSugarVsControl\tmeanSugar\tmeanControl\ttStatistic\ttValueSugar\tWithinSugar\tSugarVsControl_BW\tBodyWeightInteraction\tBodyWeight\tSugarVsControl_FoodIntake\tFoodIntakeInteraction\tFoodIntake\tSugarVsControl_Brain\tBrainInteraction\tBrain\tSugarVsControl_Adj\tWithinSugar_Adj\tSugarVsControl_BW_Adj\tBodyWeightInteraction_Adj\tBodyWeight_Adj\tSugarVsControl_FoodIntake_Adj\tFoodIntakeInteraction_Adj\tFoodIntake_Adj\tSugarVsControl_Brain_Adj\tBrainInteraction_Adj\tBrain_Adj\tBodyWeightSugarEstimate\tBodyWeightSugarInteractionEstimate\tFoodIntakeSugarEstimate\tFoodIntakeSugarInteractionEstimate",
        paste("pValueTable_SugarVsControl_",taxa,"_qiime.txt",sep=""));
  write.table(makeTable,paste("pValueTable_SugarVsControl_",taxa,"_qiime.txt",sep=""),quote=FALSE, sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE);  
}


################################################################### Make Plots ################################
setwd("C://Users//Roshonda//Dropbox//FodorLab//GoranData//statisticalModels//")
pValuesFamily <- read.delim("pValueTable_SugarVsControl_family_qiime.txt")
#pValuesFamily <- pValuesFamily[-c(1,2),]

pValuesFamily$SugarVsControl_Brain_c <- -log10(pValuesFamily$SugarVsControl_Brain)
pValuesFamily$SugarVsControl_BW_c <- -log10(pValuesFamily$SugarVsControl_BW)
pValuesFamily$SugarVsControl_FoodIntake_c  <- -log10(pValuesFamily$SugarVsControl_FoodIntake)
pValuesFamily$SugarVsControl_c <- -log10(pValuesFamily$SugarVsControl)
pValuesFamily$WithinSugar_c <- -log10(pValuesFamily$WithinSugar)

pValuesFamily$SugarVsControl_Brain_c <- ifelse(pValuesFamily$tStatistic > 0,pValuesFamily$SugarVsControl_Brain_c,-1*pValuesFamily$SugarVsControl_Brain_c)
pValuesFamily$SugarVsControl_BW_c  <- ifelse(pValuesFamily$tStatistic > 0,pValuesFamily$SugarVsControl_BW_c,-1*pValuesFamily$SugarVsControl_BW_c)
pValuesFamily$SugarVsControl_FoodIntake_c  <- ifelse(pValuesFamily$tStatistic > 0,pValuesFamily$SugarVsControl_FoodIntake_c,-1*pValuesFamily$SugarVsControl_FoodIntake_c)
pValuesFamily$SugarVsControl_c <- ifelse(pValuesFamily$tStatistic > 0,pValuesFamily$SugarVsControl_c,-1*pValuesFamily$SugarVsControl_c)
pValuesFamily$WithinSugar_c <- ifelse(pValuesFamily$tValueSugar > 0,pValuesFamily$WithinSugar_c,-1*pValuesFamily$WithinSugar_c)

pValuesFamily$BodyWeight_c <- -log10(pValuesFamily$BodyWeight)
pValuesFamily$FoodIntake_c  <- -log10(pValuesFamily$FoodIntake)
pValuesFamily$BodyWeightInteraction_c <- -log10(pValuesFamily$BodyWeightInteraction)
pValuesFamily$FoodIntakeInteraction_c  <- -log10(pValuesFamily$FoodIntakeInteraction)

pValuesFamily$BodyWeight_c  <- ifelse(pValuesFamily$BodyWeightSugarEstimate > 0,pValuesFamily$BodyWeight_c,-1*pValuesFamily$BodyWeight_c)
pValuesFamily$FoodIntake_c <- ifelse(pValuesFamily$FoodIntakeSugarEstimate > 0,pValuesFamily$FoodIntake_c,-1*pValuesFamily$FoodIntake_c)
pValuesFamily$BodyWeightInteraction_c <- ifelse(pValuesFamily$BodyWeightSugarInteractionEstimate > 0,pValuesFamily$BodyWeightInteraction_c,-1*pValuesFamily$BodyWeightInteraction_c)
pValuesFamily$FoodIntakeInteraction_c <- ifelse(pValuesFamily$FoodIntakeSugarInteractionEstimate > 0,pValuesFamily$FoodIntakeInteraction_c,-1*pValuesFamily$FoodIntakeInteraction_c)


p <- ggplot(pValuesFamily)


tiff("sugarVsControlAgainstBrain_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_Brain_c,-log10(Brain)),
               colour = ifelse(pValuesFamily$SugarVsControl_Brain_Adj < 0.1,"red",
                               ifelse(pValuesFamily$Brain_Adj < 0.1,"red","black")),
               size = ifelse(pValuesFamily$SugarVsControl_Brain_Adj < 0.1,5,
                             ifelse(pValuesFamily$Brain_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Behavioral index p-value)") +
  xlim(-8,8)+
  geom_abline(slope = 1,intercept = 0)+
  geom_abline(slope = -1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$SugarVsControl_Brain_Adj < 0.1 | pValuesFamily$Brain_Adj < 0.1),aes(SugarVsControl_Brain_c,-log10(Brain),
                  label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("sugarVsControlAgainstBrainInteraction_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_Brain_c,-log10(BrainInteraction)),
               colour = ifelse(pValuesFamily$SugarVsControl_Brain_Adj < 0.1,"red",
                               ifelse(pValuesFamily$BrainInteraction_Adj < 0.1,"red","black")),
               size = ifelse(pValuesFamily$SugarVsControl_Brain_Adj < 0.1,5,
                             ifelse(pValuesFamily$BrainInteraction_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Behavioral index interaction with sugar vs control p-value)") +
  xlim(-7,7) +
  geom_abline(slope = 1,intercept = 0)+
  geom_abline(slope = -1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$SugarVsControl_Brain_Adj < 0.1| pValuesFamily$BrainInteraction_Adj < 0.1),aes(SugarVsControl_Brain_c,-log10(BrainInteraction),
                                                                                                                          label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("sugarVsControlAgainstBodyWeight_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_BW_c,BodyWeight_c),
               colour = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$BodyWeight_Adj < 0.1,"red2","black")),
               size = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,5,
                             ifelse(pValuesFamily$BodyWeight_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight p-value)") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_BW_Adj < 0.1 | BodyWeight_Adj < 0.1),
                  aes(SugarVsControl_BW_c,BodyWeight_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()

tiff("sugarVsControlAgainstBodyWeightInteraction_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_BW_c,BodyWeightInteraction_c),
               colour = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$BodyWeightInteraction_Adj < 0.1,"red2","black")),
               size = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,5,
                             ifelse(pValuesFamily$BodyWeightInteraction_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight Interaction with Sugar Intake p-value)") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_BW_Adj < 0.1 | BodyWeightInteraction_Adj < 0.1),
                  aes(SugarVsControl_BW_c,BodyWeightInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()



tiff("sugarVsControlAgainstBodyWeight_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_BW_c,BodyWeight_c),
               colour = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$BodyWeight_Adj < 0.1,"red2","black")),
               size = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,5,
                             ifelse(pValuesFamily$BodyWeight_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight p-value)") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_BW_Adj < 0.1 | BodyWeight_Adj < 0.1),
                  aes(SugarVsControl_BW_c,BodyWeight_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()

tiff("sugarVsControlAgainstBodyWeightInteraction_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_BW_c,BodyWeightInteraction_c),
               colour = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$BodyWeightInteraction_Adj < 0.1,"red2","black")),
               size = ifelse(pValuesFamily$SugarVsControl_BW_Adj < 0.1,5,
                             ifelse(pValuesFamily$BodyWeightInteraction_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Body Weight Interaction with Sugar Intake p-value)") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_BW_Adj < 0.1 | BodyWeightInteraction_Adj < 0.1),
                  aes(SugarVsControl_BW_c,BodyWeightInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()

tiff("sugarVsControlAgainstFoodIntake_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_FoodIntake_c,FoodIntake_c),
               colour = ifelse(pValuesFamily$SugarVsControl_FoodIntake_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$FoodIntake_Adj < 0.1,"red2","black")),
               size = ifelse(pValuesFamily$SugarVsControl_FoodIntake_Adj < 0.1,5,
                             ifelse(pValuesFamily$FoodIntake_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake p-value)") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_FoodIntake_Adj < 0.1 | FoodIntake_Adj < 0.1),
                  aes(SugarVsControl_FoodIntake_c,FoodIntake_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()


tiff("sugarVsControlAgainstFoodIntakeInteraction_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_FoodIntake_c,FoodIntakeInteraction_c),
               colour = ifelse(pValuesFamily$SugarVsControl_FoodIntake_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$FoodIntakeInteraction_Adj < 0.1,"red2","black")),
               size = ifelse(pValuesFamily$SugarVsControl_FoodIntake_Adj < 0.1,5,
                             ifelse(pValuesFamily$FoodIntakeInteraction_Adj < 0.1,5,3))) +
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Energy Intake Interaction with Sugar Intake p-value)") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_FoodIntake_Adj < 0.1 | FoodIntakeInteraction_Adj < 0.1),
                  aes(SugarVsControl_FoodIntake_c,FoodIntakeInteraction_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()

tiff(paste("sugarVsControlAgainstWithinSugar_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(SugarVsControl_c,WithinSugar_c),
               colour = ifelse(pValuesFamily$SugarVsControl_Adj < 0.1,"red2",
                               ifelse(pValuesFamily$WithinSugar_Adj < 0.12,"blue3","black")),
               size = ifelse(pValuesFamily$SugarVsControl_Adj < 0.1,5,
                             ifelse(pValuesFamily$WithinSugar_Adj < 0.12,5,3))) +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  xlab("-Log10(Sugar vs Control p-value)")+ ylab("-Log10(Fructose:Glucose ratio p-value)") +
  geom_text_repel(data = filter(pValuesFamily,SugarVsControl_Adj < 0.1 | WithinSugar_Adj < 0.12),
                  aes(SugarVsControl_c,WithinSugar_c,label = Name),size=5,force=8,box.padding= unit(0.5,"lines"),fontface='bold') +
  theme_classic(base_size = 24)+
  theme(axis.line=element_line(size=1),
        axis.ticks=element_line(size=1),
        axis.text=element_text(face="bold",size=16),
        text=element_text(face="bold",size=24)
  )
dev.off()


pValuesFamily <- read.delim("pValueTable_withinSugars_family_qiime.txt")
p <- ggplot(pValuesFamily)
tiff(paste("withinSugarsAgainstBrainInteraction.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_Brain),-log10(BrainInteraction)),
               colour = ifelse(pValuesFamily$WithinSugar_Brain < 0.05,"red",
                               ifelse(pValuesFamily$BrainInteraction < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_Brain < 0.05,5,
                             ifelse(pValuesFamily$BrainInteraction < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Behavioral index interaction with sugar group p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_Brain < 0.05 | pValuesFamily$BrainInteraction < 0.05),aes(-log10(WithinSugar_Brain),-log10(BrainInteraction),
                                                                                                                                     label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff(paste("withinSugarsAgainstBrain_qiime.tiff",sep=""),width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_Brain),-log10(Brain)),
               colour = ifelse(pValuesFamily$WithinSugar_Brain < 0.05,"red",
                               ifelse(pValuesFamily$Brain < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_Brain < 0.05,5,
                             ifelse(pValuesFamily$Brain < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Behavioral index p-value)") +
  geom_abline(slope = 1,intercept = 0)+
  xlim(0,4) +
  ylim(0,4) +
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_Brain < 0.05 | pValuesFamily$Brain < 0.05),aes(-log10(WithinSugar_Brain),-log10(Brain),
                                                                                                                                  label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("withinSugarsAgainstFoodIntake_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_FoodIntake),-log10(FoodIntake)),
               colour = ifelse(pValuesFamily$WithinSugar_FoodIntake < 0.05,"red",
                               ifelse(pValuesFamily$FoodIntake < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_FoodIntake < 0.05,5,
                             ifelse(pValuesFamily$FoodIntake < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Food Intake p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_FoodIntake < 0.05 | pValuesFamily$FoodIntake < 0.05),aes(-log10(WithinSugar_FoodIntake),-log10(FoodIntake),
                                                                                                                                    label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("withinSugarsAgainstFoodIntakeInteraction_family.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_FoodIntake),-log10(FoodIntakeInteraction)),
               colour = ifelse(pValuesFamily$WithinSugar_FoodIntake < 0.05,"red",
                               ifelse(pValuesFamily$FoodIntakeInteraction < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_FoodIntake < 0.05,5,
                             ifelse(pValuesFamily$FoodIntakeInteraction < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Food intake interaction with sugar group p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_FoodIntake < 0.05 | pValuesFamily$FoodIntakeInteraction < 0.05),aes(-log10(WithinSugar_FoodIntake),-log10(FoodIntakeInteraction),
                                                                                                                                               label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("withinSugarsAgainstSugarIntake_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_SugarIntake),-log10(SugarIntake)),
               colour = ifelse(pValuesFamily$WithinSugar_SugarIntake < 0.05,"red",
                               ifelse(pValuesFamily$SugarIntake < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_SugarIntake < 0.05,5,
                             ifelse(pValuesFamily$SugarIntake < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Sugar Intake p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_SugarIntake < 0.05 | pValuesFamily$SugarIntake < 0.05),aes(-log10(WithinSugar_SugarIntake),-log10(SugarIntake),
                                                                                                                                 label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("withinSugarsAgainstSugarIntakeInteraction_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_SugarIntake),-log10(SugarIntakeInteraction)),
               colour = ifelse(pValuesFamily$WithinSugar_SugarIntake < 0.05,"red",
                               ifelse(pValuesFamily$SugarIntakeInteraction < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_SugarIntake < 0.05,5,
                             ifelse(pValuesFamily$SugarIntakeInteraction < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Sugar intake interaction with sugar group p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_SugarIntake < 0.05 | pValuesFamily$SugarIntakeInteraction < 0.05),aes(-log10(WithinSugar_SugarIntake),-log10(SugarIntakeInteraction),
                                                                                                                                            label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("withinSugarsAgainstBodyWeight_family_qiime.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_BW),-log10(BodyWeight)),
               colour = ifelse(pValuesFamily$WithinSugar_BW < 0.05,"red",
                               ifelse(pValuesFamily$BodyWeight < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_BW < 0.05,5,
                             ifelse(pValuesFamily$BodyWeight < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Body weight p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_BW < 0.05 | pValuesFamily$BodyWeight < 0.05),aes(-log10(WithinSugar_BW),-log10(BodyWeight),
                                                                                                                            label = Name)) +
  theme_bw(base_size = 16)
dev.off()

tiff("withinSugarsAgainstBodyWeightInteraction_family.tiff",width=4200,height=3200,compression="lzw",res=300);
p + geom_point(aes(-log10(WithinSugar_BW),-log10(BodyWeightInteraction)),
               colour = ifelse(pValuesFamily$WithinSugar_BW < 0.05,"red",
                               ifelse(pValuesFamily$BodyWeightInteraction < 0.05,"red","black")),
               size = ifelse(pValuesFamily$WithinSugar_BW < 0.05,5,
                             ifelse(pValuesFamily$BodyWeightInteraction < 0.05,5,3))) +
  xlab("-Log10(Sugar Group p-value)")+ ylab("-Log10(Body weight interaction with sugar group p-value)") +
  xlim(0,4) +
  ylim(0,4) +
  geom_abline(slope = 1,intercept = 0)+
  geom_text_repel(data = subset(pValuesFamily,pValuesFamily$WithinSugar_BW < 0.05 | pValuesFamily$BodyWeightInteraction < 0.05),aes(-log10(WithinSugar_BW),-log10(BodyWeightInteraction),
                                                                                                                                       label = Name)) +
  theme_bw(base_size = 16)
dev.off()