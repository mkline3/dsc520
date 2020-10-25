# Assignment: Exercise 13
# Name: Kline, Matthew
# Date: 2020-10-25

setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")

library(foreign)
library(tidyverse)
library(caret)

surgery_df <- read.arff("data/ThoraricSurgery.arff")

surgery <- surgery_df

surgery$DGN <- as.numeric(surgery$DGN)
surgery$PRE6 <- as.numeric(surgery$PRE6)
surgery$PRE7 <- as.numeric(surgery$PRE7)
surgery$PRE8 <- as.numeric(surgery$PRE8)
surgery$PRE9 <- as.numeric(surgery$PRE9)
surgery$PRE10 <- as.numeric(surgery$PRE10)
surgery$PRE11 <- as.numeric(surgery$PRE11)
surgery$PRE14 <- as.numeric(surgery$PRE14)
surgery$PRE17 <- as.numeric(surgery$PRE17)
surgery$PRE19 <- as.numeric(surgery$PRE19)
surgery$PRE25 <- as.numeric(surgery$PRE25)
surgery$PRE30 <- as.numeric(surgery$PRE30)
surgery$PRE32 <- as.numeric(surgery$PRE32)
#surgery$Risk1Yr <- as.numeric(surgery$Risk1Yr)

 
set.seed(3456)
trainIndex <- createDataPartition(surgery$Risk1Yr, p=0.67 , list=FALSE, times=1)

Train <- surgery[trainIndex,]
Test <- surgery[-trainIndex,]

fit <- glm(Risk1Yr ~ DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 +PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, data=Train , family=binomial())
summary(fit)

Test$model_prob <- predict(fit, Test, type="response")
Test <- Test  %>% mutate(model_pred = 1*(model_prob > .53) + 0,
                         visit_binary = 1*(Risk1Yr == "T") + 0)

Test <- Test %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(Test$accurate)/nrow(Test)


