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

#convert factor to numeric 
labels <- as.numeric(target_train)-1
ts_label <- as.numeric(target_test)-1


#predict buy
#data$CLASS <- factor(target_buy, levels = c(0,1))
train$CLASS <- factor(labels, levels = c(0,1))

logit <- glm(CLASS~., data = train, family=binomial)
summary(logit)

decision <- predict(logit, newdata = test,type="response")
decision_flag <- ifelse(decision>= 0.5, 1, 0)
table(test$CLASS, decision_flag, dnn=c("Actual","Predicted"))

#view variable importance plot
# mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
# xgb.plot.importance (importance_matrix = mat[1:5]) 

#plot
Allpred_flag <- decision_flag
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
#ind<-which(ts_label>0.5)
#ind_pred<-which(Allpred_flag>0.5)

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
#library(tidyquant)
p <- ggplot()+
  geom_line(data=emp, aes(x = T, y = close))+
  #  geom_candlestick(data=emp,aes(x = T, y = close, open = open, high = high, low = low, close = close))+
  geom_point(data=growtarget, aes(x = T, y = cl),color='blue', size = 1.5)+
  geom_point(data=growdecision,aes(x = T, y = cl),color='red', size = 1.5)

p + theme(legend.position="bottom", legend.title = element_blank())

