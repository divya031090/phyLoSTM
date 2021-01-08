library(plotROC)
library(ggplot2)
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}


### LSTM+CNN
labels<-c(1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0,0,0, 1,0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0)

scores<-c(1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1, 1,0,1,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0)#scores<-c(1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
result<-simple_roc(labels,scores)
library(pROC)

plot(result[,2],result[,1], type='l', col='blue', xlim=c(0,1), ylim=c(0,1), ylab="Sensitivity", xlab="1-Specificity",lwd=2)
#lines(smoothingSpline)
height = (result[,1][-1]+result[,1][-length(result[,1])])/2
width = diff(result[,2]) # = diff(rev(omspec))
sum(height*width)
grid()

### CNN
scores_CNNbasic<-c(1,1,1,1,1,0,0,1,1,1,0,1,1,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,1,0,0,0,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0,0,1, 1,0,1,1,1,1,1,0,1,1,0,1,0,1,1,0,1,1,0,0,0,0,0,1,1,0,1,1,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,1,0,0,0,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0)
result_CNNbasic=simple_roc(labels,scores_CNNbasic)
lines(result_CNNbasic[,2],result_CNNbasic[,1], type='l', col='green', xlim=c(0,1), ylim=c(0,1),lwd=2)

height1 = (result_CNNbasic[,1][-1]+result_CNNbasic[,1][-length(result_CNNbasic[,1])])/2
width1 = diff(result_CNNbasic[,2]) # = diff(rev(omspec))
sum(height1*width1)

## lasso
scores_CNNshuffle<-c(0,1,1,0,1,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,0, 1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0)
result_CNNshuffle=simple_roc(labels,scores_CNNshuffle)
lines(result_CNNshuffle[,2],result_CNNshuffle[,1], type='l', col='pink', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_CNNshuffle = (result_CNNshuffle[,1][-1]+result_CNNshuffle[,1][-length(result_CNNshuffle[,1])])/2
width_CNNshuffle = diff(result_CNNshuffle[,2]) # = diff(rev(omspec))
sum(height_CNNshuffle*width_CNNshuffle)

## ridge
scores_CNNshuffle<-c(0,1,1,0,1,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,0, 1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0)
result_CNNshuffle=simple_roc(labels,scores_CNNshuffle)
lines(result_CNNshuffle[,2],result_CNNshuffle[,1], type='l', col='yellow', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_CNNshuffle = (result_CNNshuffle[,1][-1]+result_CNNshuffle[,1][-length(result_CNNshuffle[,1])])/2
width_CNNshuffle = diff(result_CNNshuffle[,2]) # = diff(rev(omspec))
sum(height_CNNshuffle*width_CNNshuffle)


###GBC
scores_RF<-c(1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,0,1,1,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0)
result_RF=simple_roc(labels,scores_RF)
lines(result_RF[,2],result_RF[,1], type='l', col='black', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_RF = (result_RF[,1][-1]+result_RF[,1][-length(result_RF[,1])])/2
width_RF = diff(result_RF[,2]) # = diff(rev(omspec))
sum(height_RF*width_RF)


###NBC
scores_RF<-c(1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0)
result_RF=simple_roc(labels,scores_RF)
lines(result_RF[,2],result_RF[,1], type='l', col='skyblue', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_RF = (result_RF[,1][-1]+result_RF[,1][-length(result_RF[,1])])/2
width_RF = diff(result_RF[,2]) # = diff(rev(omspec))
sum(height_RF*width_RF)



###RF
scores_NB<-c(1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,0,1,0,0,1,0,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1, 1,0,1,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,1,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0)#scores<-c(1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
result_NB=simple_roc(labels,scores_NB)
lines(result_NB[,2],result_NB[,1], type='l', col='red', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_NB = (result_NB[,1][-1]+result_NB[,1][-length(result_NB[,1])])/2
width_NB = diff(result_NB[,2]) # = diff(rev(omspec))
sum(height_NB*width_NB)





## SVM
scores_SVM<-c(1,1,1,1,1,0,0,0,1,1,1,1,1,0,1,1,1,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1, 1,0,1,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0)#scores<-c(1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
result_SVM=simple_roc(labels,scores_SVM)
lines(result_SVM[,2],result_SVM[,1], type='l', col='coral', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_SVM = (result_SVM[,1][-1]+result_SVM[,1][-length(result_SVM[,1])])/2
width_SVM = diff(result_SVM[,2]) # = diff(rev(omspec))
sum(height_SVM*width_SVM)

###taxon_corr
scores_CNNdis<-c(1,0,1,1,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1, 1,0,1,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,1,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0)#scores<-c(1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
result_CNNdis=simple_roc(labels,scores_CNNdis)
lines(result_CNNdis[,2],result_CNNdis[,1], type='l', col='brown', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_CNNdis = (result_CNNdis[,1][-1]+result_CNNdis[,1][-length(result_CNNdis[,1])])/2
width_CNNdis = diff(result_CNNdis[,2]) # = diff(rev(omspec))
sum(height_CNNdis*width_CNNdis)



#LSTM
scores_Lasso<-c(0,1,1,1,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1, 1,0,1,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,1,0,0,1,0,0,0,1,0,1,0,1,0,1,1,1,0)#scores<-c(1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
result_Lasso=simple_roc(labels,scores_Lasso)
lines(result_Lasso[,2],result_Lasso[,1], type='l', col='gray', xlim=c(0,1), ylim=c(0,1),lwd=2)

height_Lasso = (result_Lasso[,1][-1]+result_Lasso[,1][-length(result_Lasso[,1])])/2
width_Lasso = diff(result_Lasso[,2]) # = diff(rev(omspec))
sum(height_Lasso*width_Lasso)

par(pty="s")
par(mar=c(4.1, 4.1, 8.1, 4.1), xpd=TRUE)

legend("left", inset=c(0.6,-0.66),legend=c("LSTM+CNN","LSTM","taxoNN_corr","RF","SVM","Ridge","Lasso","GBC","NB","CNN"),
       col=c("blue","gray","brown","red","coral","yellow","pink","black","skyblue","green")
       ,lty=c(1,1,1,1,1,1),lwd=2,ncol=1,cex=0.7)


#legend("topright", inset=c(-0.45,0.2),legend=c("taxoNN_corr","taxoNN_dis","CNN_basic","CNN_shuffle","Lasso","SVM","Ridge","RF","GBC","NB"),
#   col=c("blue","brown","green","pink","black","coral","yellow","gray","skyblue","orange")
#  ,lty=c(1,1,1,1,1,1),lwd=2,ncol=1,cex=0.8)




###boxplots
##total allergy
a <- data.frame(Method=c('LSTM+taxoNN', 'LSTM', 'taxoNN_corr', 'RF', 'SVM','Ridge Regression','Lasso Regression', 'GBC', 'NB','CNN'),
                Mean = c(0.745,0.719,0.700,0.645,0.620,0.610,0.605,0.604,0.611, 0.572),
                Lower_lim= c(0.741,0.714,0.692,0.641,0.612,0.595,0.593,0.596,0.608,0.566),
                Upper_lim=c(0.748,0.724,0.707,0.649,0.625,0.617,0.610,0.609,0.615,0.577),
                Methods=c('Random Forest', 'Support Vector machines', 'Gaussian Bayes Classifier', 'Naive Bayes','Lasso Regression', 'Ridge Regression','CNN_basic','CNN_shuffle','taxoNN_dis','taxoNN_corr'))


##egg allergy
a <- data.frame(Method=c('LSTM+taxoNN', 'LSTM', 'taxoNN_corr', 'RF', 'SVM','Ridge Regression','Lasso Regression', 'GBC', 'NB','CNN'),
                Mean = c(0.665,0.652,0.640,0.623,0.606,0.603,0.605,0.573,0.564, 0.542),
                Lower_lim= c(0.659,0.648,0.636,0.619,0.600,0.595,0.597,0.569,0.560,0.537),
                Upper_lim=c(0.670,0.655,0.644,0.626,0.610,0.607,0.610,0.580,0.569,0.546),
                Methods=c('Random Forest', 'Support Vector machines', 'Gaussian Bayes Classifier', 'Naive Bayes','Lasso Regression', 'Ridge Regression','CNN_basic','CNN_shuffle','taxoNN_dis','taxoNN_corr'))

##peanut allergy
a <- data.frame(Method=c('LSTM+taxoNN', 'LSTM', 'taxoNN_corr', 'RF', 'SVM','Ridge Regression','Lasso Regression', 'GBC', 'NB','CNN'),
                Mean = c(0.559,0.557,0.553,0.539,0.532,0.541,0.538,0.521,0.518, 0.520),
                Lower_lim= c(0.552,0.550,0.550,0.531,0.526,0.547,0.544,0.517,0.511,0.515),
                Upper_lim=c(0.564,0.563,0.558,0.545,0.539,0.536,0.532,0.526,0.524,0.526),
                Methods=c('Random Forest', 'Support Vector machines', 'Gaussian Bayes Classifier', 'Naive Bayes','Lasso Regression', 'Ridge Regression','CNN_basic','CNN_shuffle','taxoNN_dis','taxoNN_corr'))



#Milk data
a <- data.frame(Method=c('LSTM+taxoNN', 'LSTM', 'taxoNN_corr', 'RF', 'SVM','Ridge Regression','Lasso Regression', 'GBC', 'NB','CNN'),
                Mean = c(0.735,0.709,0.687,0.637,0.605,0.584,0.576,0.580,0.593, 0.561),
                Lower_lim= c(0.728,0.700,0.681,0.629,0.591,0.578,0.570,0.572,0.584,0.556),
                Upper_lim=c(0.743,0.716,0.693,0.646,0.610,0.589,0.581,0.584,0.606,0.567),
                Methods=c('Random Forest', 'Support Vector machines', 'Gaussian Bayes Classifier', 'Naive Bayes','Lasso Regression', 'Ridge Regression','CNN_basic','CNN_shuffle','taxoNN_dis','taxoNN_corr'))

#('taxoNN_corr', 'CNN_basic', 'CNN_shuffle', 'RF','Lasso Regression', 'Ridge Regression','NB','GBC','SVM'))
level_order <- c('CNN','NB','GBC','Lasso Regression','Ridge Regression','SVM','RF','taxoNN_corr', 'LSTM',  'LSTM+taxoNN')
library(ggplot2)

p<-ggplot(a,                ### The data frame to use.
          aes(x = factor(Method, levels=level_order),
              y = Mean,
              color = level_order)) +
  geom_errorbar(aes(ymin = Lower_lim,
                    ymax = Upper_lim),
                width = 0.2, 
                size  = 0.9) +
  geom_point(shape = 5, 
             size  = 1.5) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  
  theme(aspect.ratio = 1, text = element_text(size=18))+
  ylim(0.5,0.8) +
  labs(y="AUC", x = "")
#p + theme(legend.position = "none")
z<-p+coord_flip()
z + theme(legend.position = "none")

#increasing trend for time series

x=c(1,2,3,3)
y=c(0.712,0.706,0.701,0.735)
plot(x,y,type = "b",pch=19,col="blue")

dat <- read.table(text = "A   B
  0.712 0
  0.706 0
  0.701 0.735", header = TRUE)

counts=(table(dat$A,dat$B))

barplot(dat$A, col=rgb(1, 0, 0)) 
barplot(dat$B, col=rgb(0, 0, 0.5), beside=TRUE) 
barplot(dat,beside=TRUE)
legend('top', bty = 'n', title = 'Legend',
       legend = c('A', 'B'), fill = c('red', 'green'))


