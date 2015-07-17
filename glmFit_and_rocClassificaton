# the input file must contain
# - a column 1 in which there are the values of the predictor variable
# - a column 2 in which there are a code corresponding to the category (2 possible categories only, thus 2 codes only, which must be coded 0 and 1) 

# Convention: in the column 2 the code of the category in which there are the uppest values of comlumn 1 must be coded 1, while the category in which there are the lowest values of comlumn 1 will be coded 0
# e.g. : in infralittoral values of light are upper than in circalittoral. Thus the code for infralittoral will have to be 1 and the code for circalittoral will be 0

# the model is a glm which will allow, given values of the predictor (eg light), to predict probalities of occurence of category having the value 1 (eg infralittoral)

rm(list=ls())
gc()




#--------------------------------------------------------------------------------

source("utilities.R")

strGroundtruthPath<-"Y:/EUSeaMap2/thresholds/data/UK/20150616_ToIfremer"
strGroundtruthFile<-"KelpUKNorwayV2.csv"
#strGroundtruthFile<-"201502MR_UK_VJune2015.csv"
#strGroundtruthFile<-"201502MR_UK50m_VJune2015.csv"
#strGroundtruthFile<-"20150612_AllKelpNorwayPA.csv"
#strGroundtruthFile<-"KelpUKNorwayV2SelectedRecords.csv"

strTitle<-"fraction of surface light available at the seabed"
strUnit<-"" #eg mol.pho.m-2.d-1
strValueFieldName<-"Fr"
strCategoryFieldName<-"photicZ"

#parameters used for the plot of the GLM curve 
#(min, max, and x intervals of points to be drawn on the curve)
glmCurveDrawMin<-0
glmCurveDrawMax<-0.07
glmCurveDrawInterval<-0.001

strCat1Name<-"infralittoral"
strCat2Name<-"circalittoral"

#proportion of dataset used for fitting the model
#1-train_percentage will be used for validation and determination of the cutoff value
#if train_percentage=1 the model will be fitted and the probability cutoff will be worked out with all the dataset
train_percentage<-0.70
#train_percentage<-1

#will the outliers be deleted from the dataset ?
#This is an automatical process, ie all samples that are outside each category boxplot are deleted
removeOutliers<-TRUE

removeDuplicates<-FALSE

strROCThresholdMethod<-"se=sp"

#--------------------------------------------------------------------------------

t<-read.csv(paste(strGroundtruthPath,strGroundtruthFile,sep="/"))
OrigicalRowCount<-NROW(t)


# Remove duplicates
duplicatedRows<-0
if (removeDuplicates) {
  duplicatedRows<-NROW(t[duplicated(t[[strValueFieldName]]),])
  t<-t[!duplicated(t[[strValueFieldName]]),]
}

totalOutliers<-0
if (removeOutliers) {
  outputs<-removeOutliersForEachCategory (t,strCategoryFieldName,strValueFieldName,0:1)
  t<-outputs[[1]]
  totalOutliers<-outputs[[2]]
}

#boxplot
strBoxPlotTitle <-paste("Distribution of",strTitle) 
boxplot(t[[strValueFieldName]]~t[[strCategoryFieldName]],ylab=strTitle,names=c(strCat2Name,strCat1Name),main=strBoxPlotTitle)

#splitting the dataset into a train set and a test set
if (train_percentage!=1) {
  train_ind<-getRandomTrainIndexes(nrow(t),train_percentage)
  train <- t[train_ind, ]
  test <- t[-train_ind, ]
  test_percentage<-1-train_percentage
} else {
  #train_percentage == 1
  train<-t
  test<-t
  test_percentage<-1
}

#formatting the train and test tables
train<-data.frame(category=train[[strCategoryFieldName]],
                  predictor=train[[strValueFieldName]])

test<-data.frame(category=test[[strCategoryFieldName]],
                 predictor=test[[strValueFieldName]])
  
#fitting the model
model<-glm(formula=category~predictor,data=train,family=binomial(link="logit"))

#validating the model and picking up the best probability cutoff through a ROC analysis of the test dataset 
yhat<-predict.glm(model,newdata=test,type="response")
yhat<-cbind(test,yhat)
outputs<-ROCAnalysis(yhat,"category","yhat",strCat1Name,strCat2Name,strROCThresholdMethod)
AUC<-outputs[[1]]
bestProbaCutoff<-outputs[[2]]

beta0<-model$coefficients[1]
beta1<-model$coefficients[2]
#calculatin the predictor value that corresponds to the probability cut-off 
bestPredictorCutoff<-(log(bestProbaCutoff/(1-bestProbaCutoff))-beta0)/beta1

#report all
strExp<-paste("exp(",round(beta0,3),"+",round(beta1,3),"*",strValueFieldName,")",sep="")
print(paste("The original dataset contained",OrigicalRowCount,"rows"))
print(paste("of which",duplicatedRows,"duplicates were removed"))
print(paste("and of which",totalOutliers,"outliers were removed"))
print(paste("Therefore the analysis was carried out with a dataset of",NROW(t),"rows"))
print(paste("GLM model was fitted with",train_percentage*100,"% of the dataset"))
summary(model)
print(paste("The fraction of deviance explained is",1-(model$deviance/model$null.deviance)))
print(paste("ROC analysis was carried out with",test_percentage*100,"% of the dataset"))
print(paste("AUC (area under the ROC curve) is",AUC))
print(paste("optimal probability threshold is ",bestProbaCutoff," (obtained via ",strROCThresholdMethod," method)",sep=""))
print(paste("which correspond to a value of",bestPredictorCutoff,strUnit,"of", strTitle))
print(paste("the equation to translate a value of", strValueFieldName, "into a probability of occurence of",strCat1Name,"is"))
print(paste("P(",strValueFieldName,") = ",strExp, " / (1+",strExp,")",sep=""))


#plotting the sample points together with the GLM curve

#the points
strXlab <- strTitle
if (strUnit != "") strXlab <- paste(strXlab," (",strUnit,")",sep="")
plot(test$predictor, test$category, xlim=c(glmCurveDrawMin, glmCurveDrawMax), xlab=strXlab, 
     ylab=paste("Probability of",strCat1Name,"occurence"))

#the curve
xPreditor <- seq(glmCurveDrawMin, glmCurveDrawMax, glmCurveDrawInterval)
yhatpredictor <- predict.glm(model, list(predictor = xPreditor),type="response")
lines(xPreditor,yhatpredictor)

title (main=paste("Observed occurences of ",strCat1Name," and ",strCat2Name," along a gradient of ",
                  strTitle, "\nThe curve represents the predicted occurence of ",strCat1Name, sep=""),cex.main=0.8)
