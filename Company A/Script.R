# Directory   
    setwd("D://R//job Assign//emply//Case-study1//Case-study1")
    getwd()

## library
    library(readr)
    library(dplyr)
    library(rpart)
    library(caret)
    library(data.table)
    library(e1071)
  
## read a file
    traindata<-read_csv("traindata_R.csv")
    testdata<-read_csv("testdata_R.csv")

## Summary
    str(traindata)
    str(testdata)
## Question 1 Summary of wife education     
    SummaryTable<-traindata %>%
      group_by(Wife_education) %>%  
      summarise(total = n(),
                avg.age=round(mean(Wife_age,na.rm=TRUE),2),
                avg.noofchild=round(mean(Number_of_children_ever_born,na.rm=TRUE),2),
                WorkingPercent = round(sum(Wife_working) * 100 / n(),2),
                highstanderdPercent = round(100*sum(Standard_of_living_index==4)/n(),2))

    View(SummaryTable)
    
 ## Feature Enginerring
 # Converted Number_of_children_ever_born to the family group
 # XS (extra small, 0 child)- 1
 # S (small, 1-2 child)- 2 
 # M (medium, 3-4 child)- 3 
 # L (large, 5-6 child)- 4 
 # XL (extra large, 7 or more child)- 5
 
    familybreaks <- c(0,1,3,5,7,20)
    familylabels <- c(1,2,3,4,5)
    
    setDT(traindata)[ , familygroups := cut(Number_of_children_ever_born, 
                                       breaks = familybreaks, 
                                       right = FALSE, 
                                       labels = familylabels)]
    setDT(testdata)[ , familygroups := cut(Number_of_children_ever_born, 
                                      breaks = familybreaks, 
                                      right = FALSE, 
                                      labels = familylabels)]
 
## Preparing testdata
    actual<-testdata$Party_voted_for
    testdata<-testdata[,-10]
 
## formula and predictor values
    formula<-Party_voted_for~.
  
## build a decision tree  
    treemodel<-rpart(formula,traindata, method = "class"
                     ,control=rpart.control( minsplit = 6,cp = .01))

  # predict   
    treepred<-predict(treemodel,testdata,type = "class")

  # Confusion Matrix
    confusionMatrix(treepred, actual)
  
  # Pruning 
  # NOTE:- after tree pruning the accuracy increase to .723 from .699 but I
    # prefferd treemodel without pruning
    treeModelpruning<-prune(treemodel, cp = treemodel$cptable[which.min(treemodel$cptable[,"xerror"]),"CP"])
    # predict   
    treepredpruning<-predict(treeModelpruning,testdata,type = "class")
    
    # Confusion Matrix
    confusionMatrix(treepredpruning, actual)
    
## logistic regression 
    glmmodel <- glm(formula,family=binomial(link='logit'),data=traindata)
    # summary(glmmodel)
    # anova(glmmodel, test="Chisq")
  
  # predict 
    glmpred<-predict(glmmodel,testdata,type="response")
 
  # Confusion Matrix
    glmpred<-ifelse(glmpred > 0.5,1,0)
    confusionMatrix(glmpred,actual)
  
## build a random forest
  
  # library Random Forest
    library(randomForest)
    library(caret)

  # converting to factor (RF take factor value of traget value for classification)
    traindata$Party_voted_for<-factor(traindata$Party_voted_for)
    actual<-factor(actual)
 
  # model
    set.seed(500)
    Randommodel<-randomForest(formula,traindata,mtry=3) 
  
  # predict
    randompred<-predict(Randommodel,testdata)
  
  # Confusion Matrix
    confusionMatrix(randompred,actual)

## ROC and AOC for all models  
  # Libraray
    library(ROCR)
  
  # Tree 
    treepred<-as.numeric(treepred)
    actual<-as.numeric(actual)
    treepr <- prediction(treepred,actual)
    treeprf <- performance(treepr, measure = "tpr", x.measure = "fpr")
    plot(treeprf,colorize=TRUE) # plot
  
  # RF
    randompred<-as.numeric(randompred)
    randompr <- prediction(randompred,actual)
    randomprf <- performance(randompr, measure = "tpr", x.measure = "fpr")
    plot(randomprf,add = TRUE,colorize=TRUE) # add plot for comparison
  
  # Logistic
    glmpr <- prediction(glmpred,actual)
    glmprf <- performance(glmpr, measure = "tpr", x.measure = "fpr")
    plot(glmprf,add = TRUE,colorize=TRUE) # add plot for comparison
  
  # calculating AUC tree
    auc <- performance(treepr,"auc")
    auc <- unlist(slot(auc, "y.values"))
    auc<-round(auc, digits = 2)
    treeauc <- paste(c("Tree AUC - "),auc,sep="") # for legend
  
  # calculating AUC RF
    auc <- performance(randompr,"auc")
    auc <- unlist(slot(auc, "y.values"))
    auc<-round(auc, digits = 2)
    rfauc <- paste(c("Rf AUC - "),auc,sep="")# for legend
  
  # calculating AUC glm
    auc <- performance(glmpr,"auc")
    auc <- unlist(slot(auc, "y.values"))
    auc<-round(auc, digits = 2)
    logauc <- paste(c("Logistic AUC - "),auc,sep="")# for legend
  
  # Legend
    legend(.6,.3,c(treeauc,rfauc,logauc),border="white",cex=1,box.col = "white")
  
  # Title
    title("ROC for different model")
  
# ROC and AOC for all models in diffrent color.
    preds <- cbind(treepred, randompred, glmpred)
    n <- 3 # you have n models
    colors <- c('red','green', 'blue') # 3 colors
    for (i in 1:n) {
      plot(performance(prediction(preds[,i],actual),"tpr","fpr"), 
           add=(i!=1),col=colors[i],lwd=3,xlim = c(0, 1),ylim = c(0, 1))
      }
    # Legend
    legend(.8,.2 , # places a legend at the appropriate place 
           c(treeauc,rfauc,logauc), # puts text in the legend
           lty=c(1,1), # gives the legend appropriate symbols (lines)
           lwd=c(.5,.5),
           col=colors)
    #Title
    title("ROC for different model")
  
  
## Notes: As Logistic is giving highest AOC, so logistic regression is best model. 
  