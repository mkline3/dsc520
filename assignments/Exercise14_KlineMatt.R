# Assignment: Exercise 13
# Name: Kline, Matthew
# Date: 2020-10-25

setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")


library(tidyverse)
library(caret)
library(class)

binary_df <- read.csv("data/binary-classifier-data.csv")

model <- glm(label ~ x + y , data= binary_df, family= binomial())

summary(model)

set.seed(3456)
trainIndex <- createDataPartition(binary_df$label, p=0.67 , list=FALSE, times=1)

Train <- binary_df[trainIndex,]
Test <- binary_df[-trainIndex,]

TrainK <- binary_df[trainIndex,]
TestK <- binary_df[-trainIndex,]

fit <- glm(label ~ x + y , data=Train , family=binomial())
summary(fit)

Test$model_prob <- predict(fit, Test, type="response")
Test <- Test  %>% mutate(model_pred = 1*(model_prob > .53) + 0,
                         visit_binary = 1*(label == 1) + 0)

Test <- Test %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(Test$accurate)/nrow(Test)


target_category <- binary_df[trainIndex, 1]

test_category <- binary_df[-trainIndex, 1]


pr <- knn(TrainK, TestK, cl=target_category, k = 6)
tab <- table(pr, test_category)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



