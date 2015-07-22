library(ROCR)

removeOutliersForEachCategory <- function(dataframe,strCategoryFieldName,strValueFieldName, catUniqueValues) {
	#For each category (given by column strCategoryFieldName), removes 
	#the outliers of the quantitative variable corresponding to the column strValueFieldName  
	#dataframe: name of the dataframe 
	#strCategoryFieldName: name of the column that contains the categorical values
	#strValueFieldName: name of the column that contains the quantitative values
	#catUniqueValues: vector of possible values for categories
	tclean<-dataframe
	print(paste("Removing outliers for",strValueFieldName,"variable..."))
	print("Values will be considered as outliers if they are outside of the boxplot...")
	totalOutliers<-0
  for (i in catUniqueValues) {
		tcat<-tclean[which(tclean[strCategoryFieldName]==i),]
		bxplt<-boxplot(tcat[strValueFieldName],plot=FALSE)
		outliers<-bxplt$out
		totalOutliers<-totalOutliers+NROW(outliers)
		tclean<-tclean[which(! tclean[,strValueFieldName]  %in% outliers),]
		print(paste(NROW(outliers),"outliers were removed","for category",i))
	}
	output<-list(tclean,totalOutliers)
	output
}


getRandomTrainIndexes<-function(nrows, percentage) {
	#split the dataset into training and test data
	smp_size <- floor(percentage * nrows)
	## set the seed to make the partition reproductible
	set.seed(123)
	train_ind <- sample(seq_len(nrows), size = smp_size)
	train_ind
}






ROCAnalysis<-function(table,strCategoryFieldName,strValueFieldName,strXCat,strYCat,strMethod) {

	
	#strMethod either "se=sp" or "max Youden"
  #if "max-youden" the optimal threshold will be the one that is the closest to the (0,1) point
  #if "se=sp" the optimal threshold will be where sensitivity (y) = specificity (1-x); read 
  #Threshold-dependence as a desirable attribute for discrimination assessment: implications for the evaluation of species distribution models - 
  #Springer, doi:10.1007/s10531-013-0606-1

  
  strTitle<-paste("Roc curve for probability of",strXCat)

	cutoffValuesPrintMin<-0
	cutoffValuesPrintMax<-1
	cutoffValuesPrintInterval<-0.1
	cutoffValuesPrintTextSize<-0.6

	pred <- prediction(table[[strValueFieldName]], table[[strCategoryFieldName]])
	perf.tpr.fpr <- performance(pred, "tpr", "fpr")
	perf.auc <- performance(pred, "auc")
  AUC=perf.auc@y.values[[1]]
  
  lst_x<-perf.tpr.fpr@x.values[[1]]
  lst_y<-perf.tpr.fpr@y.values[[1]]
  lst_cutoff<-perf.tpr.fpr@alpha.values[[1]]
  
	if (strMethod == "max Youden") {  
    #calculation of the distance with the (0,1) point
    lst_dist<-sqrt(lst_x*lst_x+(lst_y-1)*(lst_y-1))
	  indexBestCutoff<-which(lst_dist == min(lst_dist))	
  } else {
	  #strMethod == "se=sp"
	  #ie Threshold is when specificity = sensitivity
    #1-lst_x because specificity = (1-x) 
    indexBestCutoff <- which(abs(1-lst_x-lst_y) == min(abs(1-lst_x-lst_y)))
	}
  
  bestCutoff<-lst_cutoff[[indexBestCutoff]]
  x_cutoff<-lst_x[[indexBestCutoff]]
  y_cutoff<-lst_y[[indexBestCutoff]]
  
  sensitivity=lst_y[[indexBestCutoff]]
  specificity=1-lst_x[[indexBestCutoff]]

	plot(perf.tpr.fpr, main="", colorize=FALSE,
		xlab=paste("False",strYCat, "rate (i.e. 1 - True",strXCat,"rate)"), ylab=paste("True", strYCat,"rate"),
		print.cutoffs.at=seq(cutoffValuesPrintMin,cutoffValuesPrintMax,by=cutoffValuesPrintInterval),
		text.cex=cutoffValuesPrintTextSize, text.adj=c(-0.3, 1.2), lwd=2
		)

  abline(h=1, v=0, col = "green", lty=2)
  x<-c(0,1)
  y<-c(0,1)
  lines(x,y,col = "red", lty=2)
  
  x<-c(0,x_cutoff)
  y<-c(1,y_cutoff)
  
  if (strMethod == "max Youden") lines(x,y,col = "green", lty=2)
  
  
  title(main=paste(strTitle, "\nAUC is", round(AUC,digits=3), sep=" "))
	text(x_cutoff, y_cutoff, paste(round(bestCutoff,4),"------->",sep=""),col="black",adj = c(1,0), cex=0.6)
	text(0.48,0.4,paste("Optimal threshold value is ", round(bestCutoff,4), " (obtained via ",strMethod, " method)", sep=""),adj = c(0,0), cex=0.8)
	text(0.48,0.25,paste("For this value\n   -True", strYCat, "rate is", round(sensitivity,3), "\n   -True" ,strXCat, "rate is", round(specificity,3), "\n   -overall accuracy is therefore", round((specificity+sensitivity)/2,3) , sep=" "),adj = c(0,0), cex=0.8)

	text(0.25,0, paste ("N.B.:\nTrue", strYCat, "rate = Y = Rate of",strYCat,"samples correctly predicted after classification\nTrue",strXCat, "rate = (1-X) = Rate of",strXCat,"samples correctly predicted after classification" ),adj = c(0,0), cex=0.6)

	output<-list(AUC,bestCutoff)
	
	output
}



