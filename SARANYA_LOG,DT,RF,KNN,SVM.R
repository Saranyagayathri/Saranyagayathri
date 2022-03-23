chem <-read.csv(choose.files(),sep=';')
head(chem)
dim(chem)
str(chem)

# data Preprocessing 
# Step 1 
# Check whether there is a missing value or not 
colSums(is.na(chem))

# By cheking in whole we did'nt find any missing values lets check indepth to find any that are imputed as 0 or mentioned as char 
table(chem$X3.919)
table(chem$X2.6909)
table(chem$X0)
table(chem$X0.1)
table(chem$X0.2)
table(chem$X0.4)
table(chem$X31.4)
table(chem$X2)
table(chem$X0.5)
table(chem$X0.6)
table(chem$X0.7)
table(chem$X3.106)
table(chem$X2.55) 
table(chem$X9.002)
table(chem$X0.8)
table(chem$X0.96)
table(chem$X1.142)
table(chem$X0.9)
table(chem$X0.10)
table(chem$X0.11)
table(chem$X1.201)
table(chem$X0.12)
table(chem$X0.13)
table(chem$X0.14)
table(chem$X0.15)
table(chem$X1.932)
table(chem$X0.011)
table(chem$X0.16)
table(chem$X0.17)
table(chem$X4.489)
table(chem$X0.18)
table(chem$X0.19)
table(chem$X0.20)
table(chem$X0.21)
table(chem$X2.949)
table(chem$X1.591)
table(chem$X0.22)
table(chem$X7.253)
table(chem$X0.23)
table(chem$X0.24)
table(chem$RB)

Outlier check 

# as this business problem is a classification problem so there wont be any need to detect the outliers 

# step 3 : one hot encoding or label encoding 
str(chem)
dim(chem)
table(chem$RB)
View(chem$RB)
# As there were only one char variable there is no need for one hot encoder instead lets manipulate or modifiy the data like :

chem$RB <-ifelse(chem$RB=='RB',0,1)
table(chem$RB)
str(chem)
# here what i have done is instead of making it as dummy variable i have changed the char into the num 

# step 4 : Feature Scaling 

# There is no need of feature scalar as there is no large scale of data 

# step 5 : Handing the imbalance data 

table(chem$RB)
355*2
# Verified : There is no imbalance data 

# Lets Proceed With model Building 

# lets split the data into train and test 
set.seed(555)
library(caTools)
split<-sample.split(chem$RB,SplitRatio = 0.75)
split
table(split)
# Train data 
train <- subset(chem,split==T)
# Test data 
test <- subset(chem,split==F)

# Model Building 

# Model:1 --> Logistic Regression with Hyperparameter tunning 

library(caret)
modelLookup('glm')

help(glm)
log <-glm(RB~.,family = 'binomial',data=chem)
log
summary(log)

pred <-predict(log,newdata=test,type='response')
pred

pred_50 <-ifelse(pred>0.50,1,0)
pred_50
cm <-table(test$RB,pred_50)
confusionMatrix(cm)


# Lets print the Auc - ROC curve 

library(ROCR)
ROCRpredict <-prediction(test$RB,pred_50)
ROCRpredict
ROCRvalue <-performance(ROCRpredict,'tpr','fpr')
plot(ROCRvalue)
abline(a=0,b=1)

# Lets Print the precision and Recall 
#error metrics -- Confusion Matrix

error_metric=function(CM)
{
  
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  precision =(TP)/(TP+FP)
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  sensitivity_model = (TP)/(TP+FN)
  print(paste("Precision value of the model: ",round(precision,2)))
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  print(paste('Sensitivity_of_the model',round(sensitivity_model,2)))
  
  
}


error_metric(cm)


# Model_2 
# Decision Tree 
# Lets Import the library for Decision Tree 

library(rpart)
decisionTree <- rpart(RB~.,data=train)
decisionTree
summary(decisionTree)
plot(decisionTree)
text(decisionTree)
library(rattle)
fancyRpartPlot(decisionTree)

prediction_dt <- predict(decisionTree,newdata=test)
prediction_dt

prediction_50 <-ifelse(prediction_dt>=0.5,1,0)
prediction_50
cm <-table(test$RB,prediction_50)
confusionMatrix(cm)

error_metric(cm)


ROCRpredict<-prediction(test$RB,prediction_50)
ROCRvalue<-performance(ROCRpredict,'tpr','fpr')
plot(ROCRvalue)
abline(a=0,b=1)

# Model _ 3 
library(randomForest)
randomFo<-randomForest(RB~.,data=train)
summary(randomFo)
pred <-predict(randomFo,newdata=test)
pred

pred_50_rf <-ifelse(pred>0.5,1,0)
cm <-table(test$RB,pred_50_rf)
confusionMatrix(cm)

error_metric(cm)

# Model 3 Knn 
library(class)
library(e1071)

knn_algo <-knn(train = train , test=test,cl= train$RB , k=5)
knn_algo

cm <-table(test$RB,knn_algo)
confusionMatrix(cm)

error_metric(cm)

#Model 4 
# SVM
svm <-svm(RB~.,data=train,kernel='linear')
svm
svm_prediction <-predict(svm,newdata=test)
svm_prediction

svm_prediction_50 <- ifelse(svm_prediction>=0.5,1,0)
svm_prediction_50

# Confusion Matrix 
cm <-table(test$RB,svm_prediction_50)
confusionMatrix(cm)

error_metric(cm)
# logistic regression Accuracy : 0.8409 
# desicion tree Accuracy : 0.8409 
# random forest Accuracy : 0.9091  
# knn Accuracy : 0.8485 
# svm Accuracy : 0.8485

# based on accuracy measure random forest is better one 
