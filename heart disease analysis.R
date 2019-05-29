install.packages("irr")
install.packages("e1071")
install.packages("ROCR")

library(ROCR)
library(gains)
library(dplyr)
library(irr)
library(caret)
library(e1071)
setwd("C:\\Users\\Admin\\Desktop\\dhanu\\Kaggle\\heart disease")
hd<-read.csv("heart disease.csv")
summary(hd)
head(hd)

#Data Preaprtion
#Age
sum(is.na(hd$age))
boxplot(hd$age)
summary(hd$age)
hist(hd$age)
#treatment not required as it is already noramlly distrubuted

#Sex
sum(is.na(hd$sex))
summary(hd$sex)
hd$sex<-as.factor(hd$sex)
summary(hd$sex)
#treatment not required

#cp-chest pain type
sum(is.na(hd$cp))
summary(hd$cp)
hd$cp<-as.factor(hd$cp)
summary(hd$cp)
#treatment not required

#trestbps-resting blood pressure in mm Hg
sum(is.na(hd$trestbps))
summary(hd$trestbps)
boxplot(hd$trestbps)
#IQR
1.5*(140-120)
quantile(hd$trestbps,p=c(1:100)/100)
hd$trestbps<-ifelse(hd$trestbps>=177.84,130,hd$trestbps)
boxplot(hd$trestbps)
summary(hd$trestbps)

#chol-serum cholestoral in mg/dl
sum(is.na(hd$chol))
summary(hd$chol)
boxplot(hd$chol)
#IQR
1.5*(274.5-211)
quantile(hd$chol,p=c(1:100)/100)
hd$chol<-ifelse(hd$chol>=353.96,240,hd$chol)
boxplot(hd$chol)
summary(hd$chol)


#fbs- fasting blood sugar
sum(is.na(hd$fbs))
summary(hd$fbs)
hd$fbs<-as.factor(hd$fbs)
summary(hd$fbs)
#treatment not required

#restecg- resting electrocardiographic results
sum(is.na(hd$restecg))
summary(hd$restecg)
hd$restecg<-as.factor(hd$restecg)
summary(hd$restecg)

#thalach-maximum heart rate achieved
sum(is.na(hd$thalach))
summary(hd$thalach)
boxplot(hd$thalach)
hist(hd$thalach)
quantile(hd$thalach,p=c(1:100)/100)
hd$thalach<-ifelse(hd$thalach<=95,153.0,hd$thalach)
boxplot(hd$thalach)
hist(hd$thalach)
summary(hd$thalach)

#exang-exercise induced angina
sum(is.na(hd$exang))
summary(hd$exang)
hd$exang<-as.factor(hd$exang)
summary(hd$exang)

#oldpeak-ST depression induced by exercise relative to rest
sum(is.na(hd$oldpeak))
summary(hd$oldpeak)
boxplot(hd$oldpeak)
hist(hd$oldpeak)
quantile(hd$oldpeak,p=c(1:100)/100)
hd$oldpeak<-ifelse(hd$oldpeak>=4,0.80,hd$oldpeak)
boxplot(hd$oldpeak)
hist(hd$oldpeak)

#slope- the slope of the peak exercise ST segment
sum(is.na(hd$slope))
summary(hd$slope)
hd$slope<-as.factor(hd$slope)
summary(hd$slope)

#ca- number of major vessels (0-3) colored by flourosopy
sum(is.na(hd$ca))
summary(hd$ca)
hd$ca<-as.factor(hd$ca)
summary(hd$ca)

#thal
sum(is.na(hd$thal))
summary(hd$thal)
hd$thal<-as.factor(hd$thal)
summary(hd$thal)

#target
summary(hd$target)
hd$target<-as.factor(hd$target)
summary(hd$target)
 
head(hd)
#hd<-hd[-14]
head(hd)


#Splitting into test and training samples
set.seed(50)
index<-sample(nrow(hd),0.70*nrow(hd),replace=F)
train<-hd[index,]
test<-hd[-index,]

head(train)
head(test)

dim(train)
table(train$target)

#Build the first model using all the variables 

names(train)
mod1<-glm(target~.,data=train,family="binomial")
summary(mod1)

#creating dummies
train$sex1<-ifelse(train$sex==1,1,0)
test$sex1<-ifelse(test$sex==1,1,0)

train$cp2<-ifelse(train$cp==2,1,0)
test$cp2<-ifelse(test$cp==2,1,0)
train$cp3<-ifelse(train$cp==3,1,0)
test$cp3<-ifelse(test$cp==3,1,0)

train$ca1<-ifelse(train$ca==1,1,0)
test$ca1<-ifelse(test$ca==1,1,0)
train$ca2<-ifelse(train$ca==2,1,0)
test$ca2<-ifelse(test$ca==2,1,0)

mod2<-glm(target~ sex1+cp2+cp3+ca1+ca2,family="binomial",data=train)
summary(mod2)

step(mod1, direction = "both")

mod3<-glm(formula = target ~ sex + cp + trestbps + oldpeak + slope + 
            ca + thal,data = train, family = "binomial" )
summary(mod3)
#check AUC
pred<-predict(mod3, type = "response",newdata = test)
head(pred)
table(hd$target)/nrow(hd)
pred<-ifelse(pred>=.54,1,0)
#confusion matrix

pred<-as.factor(pred)
confusionMatrix(pred, test$target,positive = '1')



pred<-prediction(predict(mod3, type = 'response', newdata = test),test$target)
perf<-performance(pred, "auc")
auc<-unlist(perf@y.values)
auc  
 
#mod4<-glm(target~ sex1+cp2+cp3+oldpeak+ca1+ca2,family="binomial",data=train)
#summary(mod4)
 
