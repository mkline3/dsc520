# Assignment: Final project
# Name: Kline, Matthew
# Date: 2020-11-13

setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")


library(tidyverse)
library(caret)
library(class)
library(ggplot2)
library(cluster)

student_df <- read.csv("data/student-por.csv")

factor_df <- student_df

for (i in colnames(factor_df)){
  if(class(factor_df[[i]]) == "character"){
    factor_df[[i]] <- as.factor(factor_df[[i]])
  }
}

int_df <- factor_df

for (i in colnames(int_df)){
  if(class(int_df[[i]]) == "factor"){
    int_df[[i]] <- as.numeric(int_df[[i]])
  }
}

avg <- c()
pass <- c()
tw <- c()
passT <- c()

for( i in 1:nrow(int_df)){
  a <- (int_df$G1[i] + int_df$G2[i] + int_df$G3[i]) / 3
  avg <- c(avg, a)
  if(a > 11.8){
    pass <- c(pass, 1)
    passT <- c(passT, 'passed')
  }else{
    pass <- c(pass, 0)
    passT <- c(passT, 'failed')
  }
  if(student_df$Fjob[i] != "at_home" & student_df$Mjob[i] != "at_home" ){
    tw <- c(tw, 2)
  }else if(student_df$Fjob[i] == "at_home" & student_df$Mjob[i] == "at_home" ){
    tw <- c(tw, 0)
  }else{
    tw <-c(tw, 1)
  }
}

int_df$grade <- avg
int_df$passed <- pass
int_df$total_workers <- tw
student_df$grade <- avg
student_df$passed <- passT

head(student_df)
head(int_df)
summary(student_df)

cor(int_df$passed, int_df)
cov(int_df$passed, int_df)

ofit <- glm(passed ~ school + sex + age + address + Medu + Fedu + guardian + traveltime + studytime + failures + higher + internet + Dalc + Walc + health + absences, data=int_df , family=binomial())
summary(ofit)


set.seed(3456)
trainIndex <- createDataPartition(int_df$passed, p=0.67 , list=FALSE, times=1)

Train <- int_df[trainIndex,]
Test <- int_df[-trainIndex,]

fit <- glm(passed ~ school + sex + age + address + Medu + Fedu + guardian + traveltime + studytime + failures + higher + internet + Dalc + Walc + health + absences, data=Train , family=binomial())
summary(fit)

Test$model_prob <- predict(fit, Test, type="response")
Test <- Test  %>% mutate(model_pred = 1*(model_prob > .53) + 0,
                         visit_binary = 1*(passed == 1) + 0)

Test <- Test %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(Test$accurate)/nrow(Test)


#Including G1
fitG1 <- glm(passed ~ school + sex + age + address + Medu + Fedu + guardian + traveltime + studytime + failures + higher + internet + Dalc + Walc + health + absences + G1, data=Train , family=binomial())
Test$model_probG1 <- predict(fitG1, Test, type="response")
Test <- Test  %>% mutate(model_predG1 = 1*(model_probG1 > .53) + 0,
                         visit_binary = 1*(passed == 1) + 0)
Test <- Test %>% mutate(accurateG1 = 1*(model_predG1 == visit_binary))
sum(Test$accurateG1)/nrow(Test)
#Including G2
fitG2 <- glm(passed ~ school + sex + age + address + Medu + Fedu + guardian + traveltime + studytime + failures + higher + internet + Dalc + Walc + health + absences + G2, data=Train , family=binomial())
Test$model_probG2 <- predict(fitG2, Test, type="response")
Test <- Test  %>% mutate(model_predG2 = 1*(model_probG2 > .53) + 0,
                         visit_binary = 1*(passed == 1) + 0)
Test <- Test %>% mutate(accurateG2 = 1*(model_predG2 == visit_binary))
sum(Test$accurateG2)/nrow(Test)

#Including G3
fitG3 <- glm(passed ~ school + sex + age + address + Medu + Fedu + guardian + traveltime + studytime + failures + higher + internet + Dalc + Walc + health + absences + G3, data=Train , family=binomial())
Test$model_probG3 <- predict(fitG3, Test, type="response")
Test <- Test  %>% mutate(model_predG3 = 1*(model_probG3 > .53) + 0,
                         visit_binary = 1*(passed == 1) + 0)
Test <- Test %>% mutate(accurateG3 = 1*(model_predG3 == visit_binary))
sum(Test$accurateG3)/nrow(Test)


#graph comparing schools
ggplot(student_df, aes(school)) + geom_bar() + ggtitle("Num of participants per school")
ggplot(student_df, aes(x=grade, color=school)) + geom_bar() + ggtitle("Students Grade based split by school")
ggplot(student_df, aes(x=grade, color=sex)) + geom_bar() + ggtitle("Students Grade by Gender")
ggplot(student_df, aes(x=studytime, color= passed)) + geom_bar() + ggtitle("Students time spent studying split by if they passed")
ggplot(student_df, aes(x=grade, color=address)) + geom_bar() + ggtitle("Students Grade split by if they live in a rural or urban location")
ggplot(student_df, aes(x=Dalc, color= passed)) + geom_bar() + ggtitle("Studentes Daily Alcohol intake split by if they passed")
ggplot(student_df, aes(x=absences, color=passed)) + geom_bar() + ggtitle("Students absences split by if they passed")
ggplot(student_df, aes(x=absences,y=grade)) + geom_point() + ggtitle("Students Grade vs Student Bbsences")
corrplot::corrplot(cor(int_df))

