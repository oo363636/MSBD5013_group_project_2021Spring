
library(catboost)	
library(caret)	
library(titanic)
train=read.csv("C:/Users/Laurrie/Desktop/kaggle/training.csv")
test=read.csv("C:/Users/Laurrie/Desktop/kaggle/testing.csv")

# process the feature of time
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

#numeric the naam
train$name=as.numeric(train$name)
test$name=as.numeric(test$name)
test["target"]=0




target=train$target
t_target=test$target
train_data=select(train,month,day,hour,min,name,Open,High,Low,Close,Volume,Quote.asset.volume,Number.of.trades,Taker.buy.base.asset.volume,target)
test_data=select(test,month,day,hour,min,name,Open,High,Low,Close,Volume,Quote.asset.volume,Number.of.trades,Taker.buy.base.asset.volume,target)
train_data=train_data[order(train_data$target),]

train_pool <- catboost.load_pool(data=train_data[2000000:4892865,-14], label = train_data[2000000:4892865,14])	
test_pool <- catboost.load_pool(data=train_data[1000000:2000000,-14], label = train_data[1000000:2000000,14])	
val_pool <- catboost.load_pool(data=train_data[1:1000000,-14], label = train_data[1:1000000,14])	


#train_data=train_data[order(train_data$target),]#reorder
fit_params <- list(iterations = 400,	
                   thread_count = 10,	
                   loss_function = 'RMSE',	
                   #ignored_features = c(4,9),	
                  # border_count = 32,	
                   depth = 6,	
                   learning_rate = 0.01,	
                   l2_leaf_reg = 3.5,	
                   #train_dir = 'train_dir',	
                   logging_level = 'Verbose'	
)	
model_1<- catboost.train(train_pool,test_pool,fit_params)	
model_2<- catboost.train(train_pool,val_pool,fit_params)	













