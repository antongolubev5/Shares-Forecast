library(quantmod)
library(ggplot2)
library(yuima)
library(zyp)
library(caret)
library(scales)
library(dplyr)

data <- read.csv('SBERBANK_Learning_Data_02.txt', sep = '\t') # read file , sep = ';'
data$time <- as.Date(data$time, "%d.%m.%Y")
sale <- data$sale
nothing <- data$nothing
time <- data$time
target_buy <- data$buy

# нормализация данных
data$OPEN<- rescale(data$OPEN)
data$HIGH <- rescale(data$HIGH)
data$LOW <- rescale(data$LOW)
data$CLOSE <- rescale(data$CLOSE)
data$VolMinus <- rescale(data$VolMinus)
data$VolZero <- rescale(data$VolZero)
data$VolPlus <- rescale(data$VolPlus)
data$VOL <- rescale(data$VOL)
data$imoex <- rescale(data$imoex)
data$imoex10 <- rescale(data$imoex10)

data$buy <- NULL
data$time <- NULL
data$sale <- NULL
data$nothing <- NULL

#predict buy
data$CLASS <- factor(target_buy, levels = c(0,1))
set.seed(1000)
train.idx <- createDataPartition(data$CLASS, p=0.7, list = FALSE)

logit <- glm(CLASS~., data = data[train.idx,], family=binomial)
#logit <- glm(CLASS~., data = bh[train.idx,], family=binomial)
#6. Examine the model (your results could differ because of random partitioning):
summary(logit)
#bh[-train.idx,"PROB_SUCC"] <- predict(logit, newdata = bh[-train.idx,],type="response")
data[-train.idx,"PROB_SUCC"] <- predict(logit, newdata = data[-train.idx,],type="response")
#8. Classify the cases using a cutoff probability of 0.5:
 # bh[-train.idx,"PRED_50"] <- ifelse(bh[-train.idx, "PROB_SUCC"]>= 0.5, 1, 0)
data[-train.idx,"PRED_50"] <- ifelse(data[-train.idx, "PROB_SUCC"]>= 0.35, 1, 0)

#9. Generate the error/classification-confusion matrix (your results could differ):
  #table(bh[-train.idx, "CLASS"], bh[-train.idx, "PRED_50"], dnn=c("Actual","Predicted"))
table(data[-train.idx, "CLASS"], data[-train.idx, "PRED_50"], dnn=c("Actual","Predicted"))

# predict sale
data$CLASS <- factor(sale, levels = c(0,1))
set.seed(1000)
train.idx <- createDataPartition(data$CLASS, p=0.7, list = FALSE)

logit <- glm(CLASS~., data = data[train.idx,], family=binomial)
#logit <- glm(CLASS~., data = bh[train.idx,], family=binomial)
#6. Examine the model (your results could differ because of random partitioning):
summary(logit)
#bh[-train.idx,"PROB_SUCC"] <- predict(logit, newdata = bh[-train.idx,],type="response")
data[-train.idx,"PROB_SUCC"] <- predict(logit, newdata = data[-train.idx,],type="response")
#8. Classify the cases using a cutoff probability of 0.5:
# bh[-train.idx,"PRED_50"] <- ifelse(bh[-train.idx, "PROB_SUCC"]>= 0.5, 1, 0)
data[-train.idx,"PRED_50"] <- ifelse(data[-train.idx, "PROB_SUCC"]>= 0.5, 1, 0)

#9. Generate the error/classification-confusion matrix (your results could differ):
#table(bh[-train.idx, "CLASS"], bh[-train.idx, "PRED_50"], dnn=c("Actual","Predicted"))
table(data[-train.idx, "CLASS"], data[-train.idx, "PRED_50"], dnn=c("Actual","Predicted"))

#plot all sale
Allpred <- predict(logit, newdata = data,type="response")
Allpred_flag <- ifelse(Allpred>= 0.35, 1, 0)
unitedflafs=Allpred_flag*10+sale
patterns<- case_when(
  unitedflafs == 0 ~ "true nothing",
  unitedflafs==1~ "false nothing",
  unitedflafs == 11~ "true sale",
  TRUE ~ "false sale"
)
emp.data <- data.frame(
  rate = time,
  close = data$CLOSE,
  Smile = patterns
)
ggplot(emp.data) + geom_point(aes(x = rate, y = close, color = Smile))+geom_line(aes(x = rate, y = close)) 

sberrate = c(5.00, 5.50, 6.00, 6.50, 7.00, 7.50, 8.00, 8.50, 9.00, 9.50, 10.00, 10.50, 11.00, 11.50, 12.00, 12.50, 13.00)
sbervolat = c(0.0137, 0.0131, 0.0128, 0.0131, 0.014, 0.0155, 0.0173, 0.0191, 0.021, 0.0228, 0.0245, 0.0262, 0.0279, 0.0295, 0.0312, 0.0327, 0.0327)
S <- sberrate*1.25
volat<-sbervolat*1.5
stringsAsFactors = FALSE
emp.data <- data.frame(
  rate = append(S, sberrate, after = length(S)),
  volatility = append(volat, sbervolat, after = length(volat)),
  Smile = append(rep("Estimation", length(S)), rep("Sber", length(sberrate)))
)
# Print the data frame.
ggplot(emp.data) + geom_point(aes(x = rate, y = volatility, color = Smile))+geom_line(aes(x = rate, y = volatility)) 