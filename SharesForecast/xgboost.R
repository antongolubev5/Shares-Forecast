library(quantmod)
library(ggplot2)
library(yuima)
library(zyp)
library(caret)
library(scales)
library(dplyr)
library(data.table)
library(mlr)
library(caret)
library(xgboost)

data <- read.csv('SBERBANK_Learning_Data_02_lookback_next.txt', sep = '\t') # read file , sep = ';'
data$time <- as.Date(data$time, "%d.%m.%Y")
sale <- data$sale
nothing <- data$nothing
time <- data$time
target_buy <- data$buy

# candle plot
p <- ggplot()+
  geom_candlestick(data=head(data, 50),aes(x = time , y = CLOSE, open = OPEN, high = HIGH, 
                                low = LOW, close = CLOSE)) +
  xlab("") + 
  ylab("") +
  theme_tq()

# data normalization
for (i in colnames(data)){
 data[[i]] <- rescale(data[[i]])
}

data$buy <- NULL
data$time <- NULL
data$sale <- NULL
data$nothing <- NULL

#predict buy
data$CLASS <- factor(target_buy, levels = c(0,1))
train.idx<-seq(1,round(length(data$CLASS)*0.7),1)

#load data 
train <- data[train.idx,]
test <- data[-train.idx,]

target_train=train$CLASS
target_test=test$CLASS

new_tr <- model.matrix(~.+0,data = train[,-c(length(colnames(data))[1])]) #[,-c("CLASS"),with=F]
new_ts <- model.matrix(~.+0,data = test[,-c(length(colnames(data))[1])])#[,-c("CLASS"),with=F]

#convert data frame to data table
setDT(train[-length(colnames(data))[1]]) 
setDT(test[-length(colnames(data))[1]])

#convert factor to numeric 
labels <- as.numeric(target_train)-1
ts_label <- as.numeric(target_test)-1

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label =labels) #,label = labels
dtest <- xgb.DMatrix(data = new_ts,label =ts_label )#,label=ts_label

#preparing model
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 79, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")

#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- as.numeric(ifelse (xgbpred > 0.4,1,0))

#confusion matrix
confusionMatrix(factor(xgbpred, levels = c(0,1)),factor(ts_label, levels = c(0,1)) )

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:5]) 


#plot
Allpred <- xgbpred <- predict (xgb1,dtest)
Allpred_flag <- ifelse(Allpred>= 0.5, 1, 0)
unitedflafs=Allpred_flag*10+ts_label

patterns<- case_when(
  #unitedflafs == 0 ~ "true nothing",
  #unitedflafs==1~ "false nothing",
  unitedflafs == 11~ "true grows prediction",
  unitedflafs == 10~ "false grows prediction",
  TRUE ~ "0"
)

emp <- data.frame(
  T = time[-train.idx],
  open=test$OPEN,
  close = test$CLOSE,
  high= test$HIGH,
  low= test$LOW,
  Smile = patterns
)

ind<-which(unitedflafs==11)
ind_pred<-which(unitedflafs==10)

growdecision <- data.frame(
  T = emp$T[ind_pred],
  cl = test$CLOSE[ind_pred]
)

growtarget <- data.frame(
  T = emp$T[ind],
  cl = test$CLOSE[ind]
)

# plot
#library(zoom)
library(tidyquant)
  
p <- ggplot()+
 geom_line(data=emp, aes(x = T, y = close), size = 1)+
 #  geom_candlestick(data=emp,aes(x = T, y = close, open = open, high = high, low = low, close = close))+
  geom_point(data=growtarget, aes(x = T, y = cl),color='blue', size = 2)+
  geom_point(data=growdecision,aes(x = T, y = cl),color='red', size = 2.5)
 
p + theme(legend.position="bottom", legend.title = element_blank())
  
