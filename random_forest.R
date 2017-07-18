library(ggplot2)
library(randomForest)
library(stringr)
library(dplyr)
library(ROCR)
setwd("D:/Kaggle/Classification/test.csv")
clasdata<-read.csv("train.csv",header = TRUE,sep = ',')
summary(clasdata)
counts
counts<-table(clasdata$species)
barplot(counts, main="Leaf type distribution", xlab="leaf Names")


set.seed(88)

train<-read.csv("train.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)
test<-read.csv("test.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)
submission<-read.csv("sample_submission.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)

all_species<-names(submission)[2:100]

train$ground_truth

colnames(submission)
train$ground_truth<-str_detect(train$species,s)
rf <- randomForest(as.factor(ground_truth) ~ .,data = train %>% select(-species),num.trees = 300)
yhat <- predict(rf, test, type = "prob")[,2]

yhat

perf = prediction(submission[],train$species)

rf

mtry <- tuneRF(train[-2],train$species, ntreeTry=500,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)


importance(rf)
varImpPlot(rf)

for(s in all_species){
  print(s)
  train$ground_truth<-str_detect(train$species,s)
  rf <- randomForest(as.factor(ground_truth) ~ .,
                     data = train %>% select(-species),
                     num.trees = 300
  )
  yhat <- predict(rf, test, type = "prob")[,2]
  submission[, eval(s)] <- yhat
}

auc=performance(submission,"auc")

mtry<-tuneRF()

write.csv(submission, "random_forest_benchmark.csv", row.names = F, quote = F)

