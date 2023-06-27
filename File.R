library(pROC)
install.packages("pROC")
library(randomForest)
install.packages("randomForest")
#randomForest is a way to classify samples and change thresholds.
set.seed(420)
num.samples <-100
weight<- sort(rnorm(n=num.samples,mean=172,sd=29))
#average man weighs 172 pounds with an sd of 29.

obese <- ifelse(test=(runif(n=num.samples)<(rank(weight)/100)), yes=1, no=0)
#rank() means ranking the weight frommlightest to heaviest
#lightest sample will have rank=1, heaviest is vice versa
#rank will change the lightest sample to .01 and heaviest to 1
obese
plot(x=weight,y=obese)
glm.fit=glm(obese ~ weight, family=binomial)
#glm.fit our stored varaible.
lines(weight,glm.fit$fitted.values)
#glm.fit$fitted.values are the y-coordinates of the weight values.
roc(obese,glm.fit$fitted.values,plot=TRUE,print.auc=TRUE,
    print.auc.x=45,partial.auc=c(100, 90),auc.polygon=TRUE,
    auc.polygon.col="blue")
#area under the curve can help us decide on which algorithm to use.

#accessing a certain part of the ROC curve
roc.info<-roc(obese,glm.fit$fitted.values,legacy.axes=TRUE)
roc.df<-data.frame(
  tpp=roc.info$sensitivities*100,
  fpp=(1-roc.info$specificities)*100,
  thresholds=roc.info$thresholds)
head(roc.df)
#*^^ When applying this above command,
#*we see that when the threshold is negative infinity,
#*the tpp is 100% and the fpp is 100% because
#*we are correctly classifying all obese people
#*but also misclassifying all non obese people.
#Correlates to the bottom right 6 rows because sensitivity is at 100%
#* because we are 100% correct but spec is 0% because we misclassified.
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
rf.model <- randomForest(factor(obese) ~ weight)

## ROC for random forest
roc(obese, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)
#Basically, we should use logistic regression over random forest for this dataset.
