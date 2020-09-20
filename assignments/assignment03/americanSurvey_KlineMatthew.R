# Assignment: American Survey Assignment
# Name: Kline, Matthew
# Date: 2020-09-20 

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")

## Load the data
survey <- read.csv("data/acs-14-1yr-s0201.csv")

summary(survey)
head(survey)

str(survey)
##Question 1: Based off of our str output: 
# Id                    : chr  
# Id2                   : int  
# Geography             : chr  
# PopGroupID            : int  
# POPGROUP.display.label: chr  
# RacesReported         : int  
# HSDegree              : num 
# BachDegree            : num

##Question 2: 
nrow(survey)
ncol(survey)

##Question 3: 
ggplot(survey, aes(HSDegree)) + geom_histogram(bins=10)+ xlab("Num of HS degree holders per county") + ylab("Num of counties") + ggtitle("HS Degree Holders Surveyed")

##Qustion 4: 
#A: Yes the data is unimodal, with the peak being between 85-90.
mean(survey$HSDegree, na.rm = TRUE)
median(survey$HSDegree, na.rm = TRUE)
# mode function found at : https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(getmode(survey$HSDegree))
#B: The graph is not symmetrical because the mode, mean, and median do not all occur at the same point.
#C: The graph does have a bell shape with only one peak.
#D: No the graph is not normal.
#E: The graph is skewed to the right.
#F: 
z<- (survey$HSDegree - mean(survey$HSDegree ))/sd(survey$HSDegree)
data <- cbind(survey, z)
d_norm <- dnorm(data$z)
data <- cbind(data, d_norm)
ggplot(data = data, aes(x = HSDegree)) + geom_histogram(bins=10) + geom_line(aes( y=d_norm), colour="red") + xlab("Num of HS degree holders per county") + ylab("Num of counties") + ggtitle("HS Degree Holders Surveyed") 
ggplot(data = data, aes(x = HSDegree)) + geom_line(aes( y=d_norm), colour="red")
#G: Yes because a normal distribuition shows the prbability at what value might occur and with the propper z scores we would see an accurate calculation.

##Question 5:
ggplot(survey, aes(sample = HSDegree)) + stat_qq()
##Question 6: 
#A: No, because the data is curved slightly on the graph instead of being a straight line.
#B: The plot is skewed to the left since the data points downs and to the right after the bend.

##Question 7:
library(pastecs)
stat.desc(survey$HSDegree)
library(e1071)
kurtosis(survey$HSDegree)
##Question 8: When looking at the graphs of the data we can see how the data is skewed. Based on the kurtosis value we can have a better understanding of the data's peak. 
#This data was skewed to the right side of the graph, showing the majority of the data was around the mean. The kurtosis value showed that this data set was too peaked due
#to the bunching of the data around the mean. Based off of our distribution graph, we can tell that the zscores fit with a normal distribution. If we had a different data size,
#this could change all these factors, our data may not have the same peak, as well as we may see a change in the normality of the distribution. This would result in changing the kurtosis, skew, and zscores.

