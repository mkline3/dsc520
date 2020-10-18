# Assignment: Housing assignement
# Name: Kline, Matthew
# Date: 2020-10-18

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")
library("readxl")

## Load the data
housing_df <- read_excel("data/week-7-housing.xlsx")

## Load the ggplot2 library
library(ggplot2)

cor(housing_df$`Sale Price`, housing_df$sale_reason)
cor(housing_df$`Sale Price`, housing_df$sale_instrument)
cor(housing_df$`Sale Price`, housing_df$zip5)
cor(housing_df$`Sale Price`, housing_df$building_grade)
cor(housing_df$`Sale Price`, housing_df$square_feet_total_living)
cor(housing_df$`Sale Price`, housing_df$bedrooms)
cor(housing_df$`Sale Price`, housing_df$bath_full_count)
cor(housing_df$`Sale Price`, housing_df$bath_half_count)
cor(housing_df$`Sale Price`, housing_df$bath_3qtr_count)
cor(housing_df$`Sale Price`, housing_df$year_built)
cor(housing_df$`Sale Price`, housing_df$year_renovated)
cor(housing_df$`Sale Price`, housing_df$sq_ft_lot)
cor(housing_df$`Sale Price`, housing_df$present_use)

library(dplyr)
housing_df <- rename(housing_df, sale_price= "Sale Price", sale_date = "Sale Date")
housing_df <- select(housing_df, -prop_type, -addr_full, -sitetype, -ctyname, -postalctyn)

basic_model <- lm(sale_price ~ sq_ft_lot - 1, data = housing_df)

multi_model <- lm(sale_price ~ 
                    sale_date + 
                    sale_reason + 
                    building_grade + 
                    square_feet_total_living + 
                    bath_3qtr_count + 
                    year_built + 
                    year_renovated + 
                    sq_ft_lot + 
                    present_use - 1, 
                  data = housing_df)
summary(basic_model)
summary(multi_model)

library(knitr)
library(lm.beta)
lm.beta(multi_model)
confint(multi_model, level = 0.90)


library(stats)
anova(basic_model)
aov(basic_model)
anova(multi_model)
aov(multi_model)

resid_df <- data.frame(standardized_residuals=rstandard(multi_model),
                 studentized_resisduals=rstudent(multi_model),
                 df_betas=dfbeta(multi_model),
                 df_fit=dffits(multi_model),
                 leverage=hatvalues(multi_model))
large_residuals <- function(value){
  if( abs(value) > 2){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

resid_df$large_residuals <- sapply(resid_df$standardized_residuals,large_residuals)
sum(subset(resid_df, large_residuals == TRUE)$standardized_residuals)

large_resid <- resid_df[resid_df$large_residuals == 'TRUE',]

cooks.distance(multi_model)
library(car)
leveragePlots(multi_model)

housing_df2 <- housing_df[,c("sale_price", "building_grade", "square_feet_total_living", "bath_3qtr_count", "sale_reason","year_built", "year_renovated", "sq_ft_lot", "present_use" ) ]


cov(housing_df2)

library(lmtest)
dwtest(formula = multi_model)


plot(multi_model)

