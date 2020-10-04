# Assignment: Student Survey exercise
# Name: Kline, Matthew
# Date: 2020-10-02

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")
## Load the `student-survey.csv` to
survey <- read.csv("data/student-survey.csv")
#A:
cov(survey)
cor(survey, method = "kendall")
cor(survey, method = "spearman")
cor(survey, method = "pearson")
cor(x = c(survey$Happines, survey$Gender), y = c(survey$TimeTV, survey$TimeReading))
cor.test(x = c(survey$Happines, survey$Gender), y = c(survey$TimeTV, survey$TimeReading), method = "pearson", conf.level = 0.99)
cor(survey)^2
#B: 
library("ggplot2")
library("ggpubr")

ggscatter(survey, x = "TimeReading", y = "TimeTV", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hours of Reading", ylab = "Hours of Tv")
ggscatter(survey, x = "TimeReading", y = "Happiness", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hours of Reading", ylab = "Happiness")
ggscatter(survey, x = "TimeReading", y = "Gender", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hours of Reading", ylab = "Gender")
ggscatter(survey, x = "Happiness", y = "TimeTV", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness", ylab = "Hours of Tv")
ggscatter(survey, x = "Gender", y = "TimeTV", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gender", ylab = "Hours of Tv")
ggscatter(survey, x = "Happiness", y = "Gender", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness", ylab = "Gender")

library(ppcor)
pcor.test( survey$TimeTV, survey$TimeReading, survey$Happiness)
reg1 = lm(survey$TimeTV ~survey$Happiness)
resid1 = resid(reg1)
reg2 = lm(survey$TimeReading ~survey$Happiness)
resid2 = resid(reg2)
cor(resid1,resid2)
