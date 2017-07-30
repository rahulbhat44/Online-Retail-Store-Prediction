---
  title: "Online Retail"
author: "Rahul Bhat"
date: "7/26/2017"
output: html_document
---
  
###Upload Data Set

Retail <- read.csv("~/Downloads/UKretail.csv", header=TRUE,na.strings = "?")

str(Retail)
summary(Retail)


###Load required libraries

library(rmarkdown)
library(knitr)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(caret)


###Change the variables to factor

factor(Retail$Country)
factor(Retail$InvoiceNo)
factor(Retail$StockCode)


###Change CustomerID to numeric

Retail$CustomerID <- as.numeric(Retail$CustomerID) 


###Get the Revenue from Quantiy * unitPrice 

Retail <- subset(Retail, Retail$Quantity>0)
Retail <- subset(Retail, Retail$UnitPrice>0)
Retail <- Retail %>% mutate(Revenue = Quantity * UnitPrice)


###Remove NA's

Retail[!complete.cases(Retail),] 
Retail <- na.omit(Retail)


###Remove time from date to make the calculations simple (library tidyr will do it)

Retail$InvoiceDate <- as.Date(Retail$InvoiceDate, format = "%Y-%m-%d %H:%M:%S")


###Aggregate the variables 

#The total amount for each transaction
totalamount <-aggregate(x = Retail$Revenue, by = list(Retail$InvoiceNo), FUN = sum)
colnames (totalamount) <- c("InvoiceNo", "TotalAmount")
summary(totalamount)

#The number of unique products ordered per transaction
unique <-aggregate(x = Retail$StockCode, by = list(Retail$InvoiceNo), FUN = length)
colnames (unique) <- c("InvoiceNo", "UniqueProducts")
summary(unique)

#Dates for each transaction
transDate <-aggregate(x = Retail$InvoiceDate, by = list(Retail$InvoiceNo), FUN = mean)
colnames (transDate) <- c("InvoiceNo", "TransactionDate")
summary(transDate)

#Aggregation by max for customer ID
customerID <-aggregate(x = Retail$CustomerID, by = list(Retail$InvoiceNo), FUN = max)
colnames (customerID) <- c("InvoiceNo", "CustomerID")
summary(customerID)


###Combining the aggregate variables and making a new data 

invaggdata <- Reduce(function(x,y) merge(x,y,all=TRUE, by="InvoiceNo"), list(totalamount, unique, transDate, customerID))
str(invaggdata)


###Aggregate CustomerID and InvoiceNo to get the Frequesncy (transactions for only 3100 customers)

custfreq <-aggregate(x = invaggdata$InvoiceNo, by = list(invaggdata$CustomerID), FUN = length)
colnames (custfreq) <- c("CustomerID", "Frequency")
str(custfreq)


#Aggregate TranscationDate and InvoiceNo to get the last transcation date

lastdate <-aggregate(x = invaggdata$TransactionDate, by = list(invaggdata$CustomerID), FUN = max)
colnames (lastdate) <- c("CustomerID", "LastTransactionDate")
str(lastdate)


#Average amount for each customer's transactions

avgamount <-aggregate(x = invaggdata$TotalAmount, by = list(invaggdata$CustomerID), FUN = mean)
colnames (avgamount) <- c("CustomerID", "AvgAmount")
str(avgamount)


#Total amount for each customer's transactions

totalamtpercust <-aggregate(x = invaggdata$TotalAmount, by = list(invaggdata$CustomerID), FUN = sum)
colnames (totalamtpercust) <- c("CustomerID", "TotalAmount")
str(totalamtpercust)


#Combining all together

custaggdata <- Reduce(function(x,y) merge(x,y,all=TRUE, by="CustomerID"), list(custfreq, lastdate, avgamount, totalamtpercust))
str(custaggdata)
summary(custaggdata)


###Let's suppose customers are not buying products from the last 5 months(July 2011 to Dec 2011) and define churn ###in that case and our data runs from 2010-12-01 to 2011-12-09


custaggdata$churn <- ifelse(custaggdata$LastTransactionDate > '2011-10-19',"0", "1")
str(custaggdata)
summary(custaggdata)


#Again change the variables churn and CustomerId to factors 

custaggdata$churn <- as.factor(as.character(custaggdata$churn))
custaggdata$CustomerID <- as.factor(as.character(custaggdata$CustomerID))


###Split the data set in to training and testing data set

set.seed(123)
trainIndex <- createDataPartition(custaggdata$churn, p = .8,list = FALSE, times = 1)
head(trainIndex)
train<- custaggdata[ trainIndex,]
test  <- custaggdata[-trainIndex,]


###Building the models using RandomForest, SVM, Xgboost and Naive Bayes

#Apply Random Forest for the cross validation in training
library(randomForest)
library(caret)
library(party) # conditional RF
library(kernlab)
train.rf <- train(churn ~ Frequency + LastTransactionDate + AvgAmount + TotalAmount, 
                  data = train, method = "rf", trControl = trainControl(method = "cv", number = 10))
train.rf
confusionMatrix(train.rf)



#Apply SVM
train.cvm <- train(churn ~ Frequency + LastTransactionDate + AvgAmount + TotalAmount, 
                   data = train, method = "svmRadialWeights", trControl = trainControl(method = "cv", number = 10))
train.cvm
confusionMatrix(train.cvm)




# Apply Xgboost
library(xgboost)
train.xgb <- train(churn ~ Frequency + LastTransactionDate + AvgAmount + TotalAmount, 
                   data = train, method = "xgbTree", trControl = trainControl(method = "cv", number = 10))
train.xgb

confusionMatrix(train.xgb)

Last model is naive bayes
library(caret)
library(klaR)
library(MASS)
train.nb <- train(churn ~ Frequency + LastTransactionDate + AvgAmount + TotalAmount, 
             data = train, method = "nb", trControl = trainControl(method = "cv", number = 10))
train.nb

confusionMatrix(train.nb)

###Results on training data from the cross validation

results <- resamples(list(RF=train.rf, XGB=train.xgb, SVM=train.cvm, NB=modelnb))
summary(results)
dotplot(results)

###Use the models on the testing data set
#RF
testPred <- predict(train.rf, test)
postResample(testPred, test$churn)
sensitivity(testPred, test$churn)
confusionMatrix(testPred, test$churn)

#SVM
testPred <- predict(train.cvm, test)
postResample(testPred, test$churn)
sensitivity(testPred, test$churn)
confusionMatrix(testPred, test$churn)

#XGB
testPred <- predict(train.xgb, test)
postResample(testPred, test$churn)
sensitivity(testPred, test$churn)
confusionMatrix(testPred, test$churn)

#NB
testPred <- predict(train.nb, test)
postResample(testPred, test$churn)
sensitivity(testPred, test$churn)
confusionMatrix(testPred, test$churn)




