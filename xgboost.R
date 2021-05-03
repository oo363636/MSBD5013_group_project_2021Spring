library(animation)
library(ggplot2)
library(Matrix)
library(lubridate)
library(xgboost)
library(dplyr)
library(stringr)
library(timeDate)
library(tidyverse)
library(nycflights13)
library(fpp2)

train=read.csv("C:/Users/Laurrie/Desktop/kaggle/training.csv")
test=read.csv("C:/Users/Laurrie/Desktop/kaggle/testing.csv")
train["time"]<-str_replace(train$time,"T"," ")
test["time"]<-str_replace(test$time,"T"," ")
train["year"]=year(train$time)-2017
train["month"]=month(train$time)
train["day"]=day(train$time)
train["hour"]=hour(train$time)
train["min"]=minute(train$time)
train["dayofweek"]=wday(train$time)

test["year"]=year(test$time)-2017
test["month"]=month(test$time)
test["day"]=day(test$time)
test["hour"]=hour(test$time)
test["min"]=minute(test$time)
test["dayofweek"]=wday(test$time)

#replace the name of stock with labels
name=sub("BTCUSDT","1",train$name)
name=sub("ETHUSDT","2",name)
name=sub("LTCUSDT","3",name)
name=sub("XRPUSDT","4",name)
train["name"]=name

name1=sub("BTCUSDT","1",test$name)
name1=sub("ETHUSDT","2",name1)
name1=sub("LTCUSDT","3",name1)
name1=sub("XRPUSDT","4",name1)
test["name"]=name1

train$name=as.numeric(train$name)
test$name=as.numeric(test$name)

test["target"]=0



target=train$target
t_target=test$target
train_data=select(train,month,day,hour,min,name,Open,High,Low,Close,Volume,Quote.asset.volume,Number.of.trades,Taker.buy.base.asset.volume,target)
test_data=select(test,month,day,hour,min,name,Open,High,Low,Close,Volume,Quote.asset.volume,Number.of.trades,Taker.buy.base.asset.volume,target)

train_data=train_data[order(train_data$target),]
set.seed(1234)

dd_train=train_data[2000000:4892865,]
train1=data.matrix(dd_train[,c(1,13)])
train2=Matrix(train1,sparse=T)
train3=dd_train[,14]
train4=list(data=train2,label=train3)
dtrain=xgb.DMatrix(data=train4$data,label=train4$label)


dd_test=train_data[1:1000000,]
test1=data.matrix(dd_test[,c(1,13)])
test2=Matrix(test1,sparse=T)
test3=dd_test[,14]
test4=list(data=test2,label=test3)
dtest=xgb.DMatrix(data=test4$data,label=test4$label)


dd_val=train_data[1000000:2000000,]
val1=data.matrix(dd_val[,c(1,13)])
val2=Matrix(val1,sparse=T)
val3=dd_val[,14]
val4=list(data=val2,label=val3)
dval=xgb.DMatrix(data=val4$data,label=val4$label)

xgb=xgboost(data=dtrain,max_depth=5,eta=0.1,nround=300)

pred_test=predict(xgb,newdata=dtest)
pred_val=predict(xgb,newdata=dval)
accuracy(pred_test, dd_test$target)
accuracy(pred_val, dd_val$target)
###########
######importance of feature based on RMSE
explainer_xgb <- explain(xgb,
                         
                         data = train4$data,
                         
                         y = train4$label,
                         
                         label = "xgboost")

explainer_xgb

vd_xgb <- variable_importance(explainer_xgb, type = "raw")

head(vd_xgb)



plot(vd_xgb)
######################prediction
test1=data.matrix(test_data[,c(1,13)])
test2=Matrix(test1,sparse=T)
test3=test_data[,14]
test4=list(data=test2,label=test3)
dtest=xgb.DMatrix(data=test4$data,label=test4$label)
pred=predict(xgb,newdata=dtest)
result=data.frame("Id"=test$id,"Predicted"=pred)
head(result)



