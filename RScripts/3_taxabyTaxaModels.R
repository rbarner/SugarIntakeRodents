rm(list=ls())

setwd("C:/Users/Roshonda/Dropbox/FodorLab/GoranData/RodentSugarIntake/data")
setwd(paste("../metaData",sep=""))
intakeData <- read.delim("recentIntakeData.txt")
metaData <- read.delim("metadata.txt")

############################################# RDP tables #######################################
taxaLevels <- c("phylum","class","order","family","genus","otuNoTaxonomy")
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
  
  abundance$SugarVsControl  <- ifelse(abundance$group %in% c("Group_1_(35F:65G)","Group_2_(65F:35G)","Group_4_(50F:50G)"),"Sugar","Control")
  abundance$sugarIntake  <- intakeData$SolutionIntake
  abundance$EnergyIntake  <- intakeData$FoodIntake
  abundance$bodyweight  <- intakeData$RatWeight
  
  sugarAbundance  <- split(abundance,abundance$SugarVsControl)$Sugar
  controlAbundance  <- split(abundance,abundance$SugarVsControl)$Control
  sugarAbundance$colors  <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","blue",ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)","purple","red"))
  sugarAbundance$sugarGroups  <- ifelse(sugarAbundance$group %in% "Group_1_(35F:65G)","35% Fruct",ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)","65% Fruct","50% Fruct"))
  sugarAbundance$groupNum <- ifelse(sugarAbundance$group %in% "Group_2_(65F:35G)",65,ifelse(sugarAbundance$group %in% "Group_4_(50F:50G)",50,35))
  
  abundance$colors  <- ifelse(abundance$SugarVsControl %in% "Control","blue3","red2")
  
  setwd("..//statisticalModels//")
  
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
  
  pValEnergyIntakeInteractionList=numeric(0);
  pValWithinSugar_EnergyIntakeList = numeric(0);
  pValEnergyIntakeList=numeric(0);
  EnergyIntakeEstimateList <- numeric(0);
  EnergyIntakeInteractionEstimateList <- numeric(0);

  foldChangeList <- numeric(0);
  tValueSugarList <- numeric(0);
  rowNames=character(0);
  lastIter=dim(abundance)[2];
  
  for(i in 1:(lastIter-8))
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
    
    model4=as.formula(paste(names(abundance)[i],"~","SugarVsControl","*","EnergyIntake"));
    print(model4);
    withinSugarwEnergyIntakeMod <- lm(model4,data=abundance);        
    pValEnergyIntakeInteraction <- anova(withinSugarwEnergyIntakeMod)$"Pr(>F)"[3];
    pValEnergyIntake <- anova(withinSugarwEnergyIntakeMod)$"Pr(>F)"[2];
    pValWithinSugar_EnergyIntake  <- anova(withinSugarwEnergyIntakeMod)$"Pr(>F)"[1];
    
    pValEnergyIntakeInteractionList[[length(pValEnergyIntakeInteractionList)+1]]  <-  pValEnergyIntakeInteraction;
    pValWithinSugar_EnergyIntakeList[[length(pValWithinSugar_EnergyIntakeList)+1]]  <-  pValWithinSugar_EnergyIntake;
    pValEnergyIntakeList[[length(pValEnergyIntakeList)+1]]  <-  pValEnergyIntake;
    EnergyIntakeEstimateList[[length(EnergyIntakeEstimateList)+1]] <- summary(withinSugarwEnergyIntakeMod)$coefficients[3,1]
    EnergyIntakeInteractionEstimateList[[length(EnergyIntakeInteractionEstimateList)+1]] <- summary(withinSugarwEnergyIntakeMod)$coefficients[4,1]

    rowNames[[length(rowNames)+1]] <- names(abundance)[i];
  }
  
  pValSugarVsControlAdj  <- p.adjust(pValSugarVsControlList, method = "fdr")
  pValWithinSugarAdj  <- p.adjust(pValWithinSugarList, method = "fdr")
  
  pValWithinSugar_BodyweightAdj  <- p.adjust(pValWithinSugar_BodyweightList, method = "fdr")
  pValBodyweightInteractionAdj  <- p.adjust(pValBodyweightInteractionList, method = "fdr")
  pValBodyweightAdj  <- p.adjust(pValBodyweightList, method = "fdr")
  
  pValWithinSugar_EnergyIntakeAdj  <- p.adjust(pValWithinSugar_EnergyIntakeList, method = "fdr")
  pValEnergyIntakeInteractionAdj  <- p.adjust(pValEnergyIntakeInteractionList, method = "fdr")
  pValEnergyIntakeAdj  <- p.adjust(pValEnergyIntakeList, method = "fdr")
  
  makeTable=data.frame(rowNames,pValSugarVsControlList,meanSugarList,meanControlList,foldChangeList*-1,tValueSugarList,pValWithinSugarList,
                       pValWithinSugar_BodyweightList,pValBodyweightInteractionList,pValBodyweightList,
                       pValWithinSugar_EnergyIntakeList,pValEnergyIntakeInteractionList,pValEnergyIntakeList,
                       pValSugarVsControlAdj,pValWithinSugarAdj,
                       pValWithinSugar_BodyweightAdj,pValBodyweightInteractionAdj,pValBodyweightAdj,
                       pValWithinSugar_EnergyIntakeAdj,pValEnergyIntakeInteractionAdj,pValEnergyIntakeAdj,
                       bodyWeightEstimateList,bodyWeightInteractionEstimateList,
                       EnergyIntakeEstimateList,EnergyIntakeInteractionEstimateList);
  write("Name\tSugarVsControl\tmeanSugar\tmeanControl\ttStatistic\ttValueSugar\tWithinSugar\tSugarVsControl_BW\tBodyWeightInteraction\tBodyWeight\tSugarVsControl_EnergyIntake\tEnergyIntakeInteraction\tEnergyIntake\tSugarVsControl_Adj\tWithinSugar_Adj\tSugarVsControl_BW_Adj\tBodyWeightInteraction_Adj\tBodyWeight_Adj\tSugarVsControl_EnergyIntake_Adj\tEnergyIntakeInteraction_Adj\tEnergyIntake_Adj\tBodyWeightSugarEstimate\tBodyWeightSugarInteractionEstimate\tEnergyIntakeSugarEstimate\tEnergyIntakeSugarInteractionEstimate",
        paste("3_",taxa,"_pValueTable_SugarVsControl.txt",sep=""));
  write.table(makeTable,paste("3_",taxa,"_pValueTable_SugarVsControl.txt",sep=""),quote=FALSE, sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE);  
}