setwd("D:/Kaggle/Otto group/test.csv")

library(randomForest)
library(stringr)
library(ROCR)
library(dplyr)
library(ggplot2)

train<-read.csv("train.csv",header = TRUE,sep = ',')
test<-read.csv("test.csv",header = TRUE,sep = ',')
submission<-read.csv("sampleSubmission.csv",header = TRUE,sep = ',')

set.seed(1)

rf<-randomForest(train[,c(-1,-95)],as.factor(train$target),ntree = 100,importance = TRUE)

mtry <- tuneRF(train[,c(-1,-95)],train$target, ntreeTry=150,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

pred<-predict(rf,test[,-1],type ="prob")

head(pred)
submission

pred<-prediction(submission,train[,95])

auc=performance(submission,"auc")


rf

varImpPlot(rf)

mtry
