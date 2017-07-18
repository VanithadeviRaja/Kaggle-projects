library(ggthemes)
library(ggplot2)
library(scales)
library(dplyr)
library(randomForest)
library(mice)

setwd("D:/Kaggle/Titanic")

train<-read.csv("train.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)
test<-read.csv("test.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)
submission<-read.csv("gender_submission.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)

total  <- bind_rows(train, test)

total$title<-gsub('(.*,)|(\\..*)','',total$Name)

table(total$Sex,total$title)



rare_title<-c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady',' Major','Rev','Sir','the Countess')
head(total)

total[total$title == 'Mlle'] <- 'Miss'
total[total$title == 'Mme'] <- 'Mrs'
total[total$title == 'Ms'] <-'Miss'
total[total$title %in% rare_title] <- 'Rare_Title'

table(total$Sex,total$title)

total$surname<-sapply(total$Name,function(x) strsplit(x,split = '[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(total$surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

total$surname

total$fsize <- total$SibSp+total$Parch+1

total$family<-paste(total$surname,total$fsize,sep = '_')

ggplot(total[,],aes(x=total$fsize, fill=factor(total$Survived)))+geom_bar(stat = 'count',position = 'dodge')+
  scale_x_continuous(breaks = c(1:11))+
  labs(x='Family Size')+
  theme_bw()

total$FsizeD[total$fsize == 1] <- 'singleton'
total$FsizeD[total$fsize < 5 & total$fsize > 1] <- 'small'
total$FsizeD[total$fsize > 4] <- 'large'

total$FsizeD

mosaicplot(table(total$FsizeD,total$Survived),main = "Family Size by Survival",shade = TRUE)


