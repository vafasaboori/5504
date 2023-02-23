# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504/Ch_17")

# Q1 ----

# part a
# build dataframe
Q1 <- data.frame(Week = c(1:12),
                  Sales = c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22))

# Technical Trading Rules Package
library(TTR)

# Moving Average 4 weeks
SMA(Q1$Sales, n = 4)

# Round to 2 decimals
round(SMA(Q1$Sales, n = 4), digits = 2)

# Last entry is the forecast for period 13. 
# We need entries 4:11 to be inserted in rows 5:12 of a new column: SMA_4
Q1$SMA_4[5:12] <- round(
                        SMA(Q1$Sales, n = 4),
                        digits = 2)[4:11]

# Errors
Q1$ErrorSq_4[5:12] <- round(
                      (Q1$Sales[5:12]-Q1$SMA_4[5:12])^2,
                      digits = 2)

# Get the sum of all columns 
colSums(Q1) # We get NA since we have missing values

# na.rm = TRUE removes any missing values 
colSums(Q1, na.rm = TRUE) 

# Alt method for Total Error Sq
sum(Q1$ErrorSq_4, na.rm = TRUE)

# MSE 4 weeks
mean(Q1$ErrorSq_4, na.rm = TRUE) # part b

# Moving Average 5 weeks
SMA(Q1$Sales, n = 5)

# Round to 2 decimals
round(SMA(Q1$Sales, n = 5), digits = 2)

# Last entry is the forecast for period 13. 
# We need entries 5:11 to be inserted in rows 6:12 of a new column: SMA_5
Q1$SMA_5[6:12] <- round(
                        SMA(Q1$Sales, n = 5),
                        digits = 2)[5:11]

# Errors
Q1$ErrorSq_5[6:12] <- round(
  (Q1$Sales[6:12]-Q1$SMA_5[6:12])^2,
  digits = 2)

# Get the sum of all columns 
colSums(Q1, na.rm = TRUE) # na.rm = TRUE removes any missing values 

# Alt method for Total Error Sq
sum(Q1$ErrorSq_5, na.rm = TRUE)

# MSE 5 weeks
mean(Q1$ErrorSq_5, na.rm = TRUE) # part b

# part c: 5 week moving average provides the smallest MSE.

# Optional: if you want to add the totals to the last row of data set
Q1 <- rbind(Q1, colSums(Q1, na.rm = TRUE))


# Q2 ----

# build the data frame
Q2 <- data.frame(Week = c(1:12),
                 Sales = c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22))

# Part a
# Exp Smoothing Alpha 0.1
EMA(Q2$Sales, n = 1, ratio = .1)

Q2$EMA_1[2:12] <- round(
                  EMA(Q2$Sales, n = 1, ratio = .1),
                  digits = 2)[1:11]
# The last entry is the forecast for period 13.

# Errors
Q2$ErrorSq_1 <- round(
                      (Q2$Sales-Q2$EMA_1)^2,
                      digits = 2)

# MSE Alpha = 0.1
mean(Q2$ErrorSq_1, na.rm = TRUE) 

# Exp Smoothing Alpha 0.2
Q2$EMA_2[2:12] <- round(
                        EMA(Q2$Sales, n = 1, ratio = .2),
                        digits = 2)[1:11]
# The last entry is the forecast for period 13.

# Errors
Q2$ErrorSq_2 <- round(
                      (Q2$Sales-Q2$EMA_2)^2,
                      digits = 2)

# MSE Alpha = 0.2
mean(Q2$ErrorSq_2, na.rm = TRUE)

# Alpha = 0.2 smoothing would be preferred based on MSE.

# Part b
# Calculating MAE for Alpha 0.1
Q2$ErrorAbs_1 <- round(
                      abs(Q2$Sales - Q2$EMA_1), 
                      digits = 2)
mean(Q2$ErrorAbs_1, na.rm = TRUE)

# Calculating MAE for Alpha 0.2
Q2$ErrorAbs_2 <- round(
                      abs(Q2$Sales - Q2$EMA_2),
                      digits = 2)

mean(Q2$ErrorAbs_2, na.rm = TRUE)
#The Alpha = 0.1 smoothing would be preferred based upon MAE.

# Part c
# Calculating MAPE for alpha = 0.1
Q2$ErrorPer_1 <- round(
                      (abs(Q2$Sales-Q2$EMA_1)/Q2$Sales), digits = 4)

round(
      mean(Q2$ErrorPer_1, na.rm = TRUE),
      digits = 4)

# Calculating MAPE for alpha = 0.2
Q2$ErrorPer_2 <- round(
                     (abs(Q2$Sales-Q2$EMA_2)/Q2$Sales), digits = 4)

round(
  mean(Q2$ErrorPer_2, na.rm = TRUE),
  digits = 4)

# Alpha = 0.1 smoothing would be preferred based upon MAPE.


# Q3 ----

# part a

# For TS plot, it's a good idea to build an actual time series
TS3 <- ts(c(80, 82, 84, 83, 83, 84, 85, 84, 82, 83, 84, 83),
          start=c(2021,1), frequency = 12)
print(TS3)
plot(TS3)

library(ggfortify)

# Autoplot for better visualization
autoplot(TS3) + xlab("Month") + ylab("% On time") # horizontal with shift at the beginning

# part b
# for manual calculations it's better to have a data frame
Q3 <- data.frame(Month = c(1:12),
                 Percentage = c(80, 82, 84, 83, 83, 84, 85, 84, 82, 83, 84, 83))


#Technical Trading Rules Package
library(TTR)

# part b
# Simple Moving Average 3 months
SMA(Q3$Percentage, n = 3) # first value is F4, and last value is F13

Q3$SMA_3[4:12] <- round(SMA(Q3$Percentage, n = 3), digits = 2)[3:11]

# plot them together

library(tidyverse)
Q3 %>% 
  ggplot(aes(Month, Percentage)) +
                geom_point() +
                geom_line() +
                geom_point(aes(Month, SMA_3)) +
                geom_line(aes(Month, SMA_3), color = "tomato")

# Part b
# MSE for 3 Month SMA
Q3$ErrorSq_3 <- round(
                  (Q3$Percentage-Q3$SMA_3)^2, digits = 2)

mean(Q3$ErrorSq_3, na.rm = TRUE)

# MSE for exponential Smoothing alpha 0.2
EMA(Q3$Percentage, n = 1, ratio = .2)
# Fist entry is F2 and last entry is the F13.

Q3$EMA_2[2:12] <- EMA(Q3$Percentage, n = 1, ratio = .2)[1:11]

Q3$ErrorSq_2 <- (Q3$Percentage - Q3$EMA_2)^2
mean((Q3$ErrorSq_2), na.rm = TRUE)

# 3 Month MA provides the most accurate forecast using MSE.

# part c
# Forecast for next month

SMA(Q3$Percentage, n = 3)[12]
# This last entry is the forecast for period 13 (Forecast done in Dec. for Jan. next year)


# Q4 ----

#Technical Trading Rules Package
library(TTR)

TS4 <- ts(c(7.35, 7.40, 7.55, 7.56, 7.60, 7.52, 7.52, 7.70, 7.62, 7.55),
         start=c(2021,1,1), 
         frequency = 52)
print(TS4)
plot(TS4) # Data pattern is horizontal, seems like trend b/c of Y scale
plot(TS4, ylim = c(0,10))

# Autoplot for better visualization
autoplot(TS4,
ylim = c(0, 10)) +
xlab("Week") +
ylab("Index") # horizontal 

# part b
# for manual calculations it's better to have a data frame
Q4 <- data.frame(Week = c(1:10),
                 Index = c(7.35, 7.40, 7.55, 7.56, 7.60, 7.52, 7.52, 7.70, 7.62, 7.55))

# Exponential Smoothing alpha 0.2
EMA(Q4$Index, n = 1, ratio = .2)
# Fist entry is F2 and last entry is the F11.

Q4$EMA_2[2:10] <- EMA(Q4$Index, n = 1, ratio = .2)[1:9]

# Errors
Q4$ErrorSq_2 <- (Q4$Index-Q4$EMA_2)^2
sum(Q4$ErrorSq_2, na.rm = TRUE)
mean(Q4$ErrorSq_2, na.rm = TRUE) # MSE Alpha 0.2

# Exponential Smoothing alpha 0.3
EMA(Q4$Index, n = 1, ratio = .3)
# Fist entry is F2 and last entry is the F11.

Q4$EMA_3[2:10] <- EMA(Q4$Index, n = 1, ratio = .3)[1:9]

# Errors
Q4$ErrorSq_3 <- (Q4$Index-Q4$EMA_3)^2
sum(Q4$ErrorSq_3, na.rm = TRUE)
mean(Q4$ErrorSq_3, na.rm = TRUE) # MSE Alpha 0.3

# Alpha 0.3 is a better model
EMA(Q4$Index, n = 1, ratio = .3)[10] # last value is the forecast for week 11

# Q5 ----

# part a
TS_5 <- ts( c(120, 110, 100, 96, 94, 92, 88),
          start=c(2021,1), 
          frequency = 12)
print(TS_5)
plot(TS_5) # Data pattern is linear

# Autoplot for better visualization
autoplot(TS_5) +
  xlab("t") +
  ylab("Yt") # linear 

# part b, Linear Regression
Q5 <- data.frame(t = c(1:7), 
                 Yt = c(120, 110, 100, 96, 94, 92, 88))
Q5_slr <- lm(Yt ~ t, data = Q5)
summary(Q5_slr)

# part c forecast for t = 8
predict(Q5_slr, list(t=8))

# note about MSE
anova(Q5_slr)
anova(Q5_slr)[2,3] # this MSE is SSE/df (df = n-k-1) different from SSE/n

# Q6 ----

# part a
TS6 <- ts(c(33.27, 44.35, 57.39, 74.76, 93.80, 117.58),
         start=c(2012), 
         frequency = 1)
print(TS6)
plot(TS6)
autoplot(TS6) # Data pattern is upward

# par b, Linear Regression
Q6 <- data.frame(year = c(1:6),
                 sub = c(33.27, 44.35, 57.39, 74.76, 93.80, 117.58))

Q6_model_1 <- lm(sub ~ year, data = Q6)
Q6_model_1$coefficients

# part c, Quadratic Regression
Q6_model_2 <- lm(sub ~ poly(year, degree = 2, raw = T), data = Q6)
Q6_model_2$coefficients

# part d, MSE Calculation

# Linear Model
anova(Q6_model_1)
anova(Q6_model_1)[2,3] # this MSE is SSE/df (df = n-k-1) different from SSE/n
anova(Q6_model_1)[2,2]/nrow(Q6) # This is SSE/n

# Quadratic Model
anova(Q6_model_2)
anova(Q6_model_2)[2,3] # this MSE is SSE/df (df = n-k-1) different from SSE/n
anova(Q6_model_2)[2,2]/nrow(Q6) # This is SSE/n

# Quadratic model appears better according to MSE.

# Part e, Forecast
predict(Q6_model_1, list(year=7))
predict(Q6_model_2, list(year=7))

# Part f, Quadratic model is preferred because of its lower MSE.

# Q7 ----

# part a, Visualization
TS7 <- ts(c(71, 49, 58, 75, 68, 41, 60, 84, 62, 51, 53, 72),
         start=c(2015,1), 
         frequency = 4) # quarterly
print(TS7)
autoplot(TS7) # Horizontal Pattern

# Part b, Regression
Q7 <- data.frame(Quarter=c(1:4), TS7) # 1:4 repeats itself

# Convert Quarter to Factor
Q7$Quarter <- as.factor(Q7$Quarter)
levels(Q7$Quarter)

# Build Dummy Variables
Q7_dummy <- model.matrix(~ Quarter -1, Q7) 
Q7 <- cbind(Q7_dummy, Q7)

# Simple Linear Regression
Q7_slr <- lm(TS7 ~ Quarter1 + Quarter2 + Quarter3, data = Q7)
summary(Q7_slr) 
predict(Q7_slr)

# Q8 ----

# part a

# build data frame
Q8 <-   c(16, 105, 168, 5,
          41, 134, 243, 34,
          73, 148, 316, 49,
          96, 204, 375, 78,
          168, 288, 448, 185)

# convert it into a time series
TS8 <- ts(Q8, start=2015, frequency=4) # quarterly

# Data is in a time series format
print(TS8)

# time series plot
plot(TS8)

# alt method using autoplot from ggfortify
library(ggfortify) # Load the ggfortify package

autoplot(TS8) + xlab("Year") + ylab("Revenue (1000)") # Upward Linear trend + Seasonal

# Dummy Variable Building

# add a column for quarters 1:4
Q8 <- data.frame(Quarter= c(1:4), Revenue = Q8)

# convert this variable to a factor
Q8$Quarter <- as.factor(Q8$Quarter)
levels(Q8$Quarter)

# create dummy variable matrix
Q8_dummy <- model.matrix(~ Quarter -1, Q8) 
Q8 <- cbind(Q8_dummy, Q8)

# Simple Linear Regression
Q8_slr_1 <- lm(Revenue ~ Quarter1 + Quarter2 + Quarter3, data = Q8)

summary(Q8_slr_1)
predict(Q8_slr_1)[1:4]

# part c
# adding Period
Q8$Period <- c(1:20)

# Simple Linear Regression with trend
Q8_slr_2 <- lm(Revenue ~ Quarter1 + Quarter2 + Quarter3 + Period, data = Q8)

# coefficients
summary(Q8_slr_2)

# forecasts
predict(Q8_slr_2, data.frame(Quarter1=1, Quarter2=0, Quarter3=0, Period=21))
predict(Q8_slr_2, data.frame(Quarter1=0, Quarter2=1, Quarter3=0, Period=22))
predict(Q8_slr_2, data.frame(Quarter1=0, Quarter2=0, Quarter3=1, Period=23))
predict(Q8_slr_2, data.frame(Quarter1=0, Quarter2=0, Quarter3=0, Period=24))

# Q9 ----

# part a

TS9 <- ts(c(1690, 940, 2625, 2500,
            1800, 900, 2900, 2360,
            1850, 1100, 2930, 2615),
         start=2015, 
         frequency=4)
print(TS9)
plot(TS9)
autoplot(TS9) # Linear Trend with Seasonal Pattern

# part b
library(fpp2) # Forecasting: Principles and Practice Ver.2

# Moving Average 4 quarters
TS9_MA_4 <- ma(TS9, order = 4, centre = F)
print(TS9_MA_4) # The last entry is the forecast for period 13.

# Centered Moving Average 4 quarters
TS9_MA_4_C <- ma(TS9, order = 4, centre = T)
print(TS9_MA_4_C)

# part c
# Seasonal Indexes (Decomposition)

# Manual:
## Step 1: TS values / centered MA = un-adjusted seasonal index for each period
## Step 2: Average un-adjusted seasonal indices for each period
## Step 3: Multiplicative model requires that sum of seasonal indices come to 4.00.
## Step 4: Adjustment: multiply indexes by the # of seasons divide by sum of the un-adjusted indexes.

# R gives you adjusted seasonal indexes.

TS9_Decomp <- decompose(TS9, type = "multiplicative" )
TS9_Decomp

autoplot(TS9_Decomp)
print(TS9_Decomp$seasonal)

# part d
# Largest seasonal index is 3rd Quarter (July, Aug, Sep) makes sense

#part e
#Calculating Deseasonalized Time Series
TS9_des <- TS9/TS9_Decomp$seasonal
print(TS9_des)

# Part f
# Using the De-seasonalized Time Series to Identify Trend
# Simple Linear Regression
Q9 <- data.frame(Period=(1:12), TS9_des)
Q9_slr <- lm(TS9_des ~ Period, data = Q9)
summary(Q9_slr)
coefficients(Q9_slr)

# Forecast Year 4
#Forecasting (4 month closed, Sep - Dec)
Q9_forecast <- predict(Q9_slr, list(Period=c(13:16)))
Q9_forecast

# Part g
# Adjusted linear trend forecasts using the adjusted seasonal indexes
Adj_Q9_forecast <- Q9_forecast * TS9_Decomp$seasonal[1:4]
print(Adj_Q9_forecast)

# Q10 ----

# part a
TS_10 <- ts(c(2750, 3100, 3250, 2800, 2900, 3050, 3300, 3100, 2950, 3000, 3200, 3150),
         start=c(2019,1,1),
         frequency = 52)

print(TS_10)
autoplot(TS_10) # Linear Trend?
autoplot(TS_10, ylim = c(0, 4000)) # No: Horizontal Pattern

# part b
# Exponential smoothing Alpha=0.4
EMA(TS_10, n = 1, ratio = .4) # The last entry is the forecast for period 13
EMA(TS_10, n = 1, ratio = .4)[12]


# Q11 ----

TS11 <- ts(c(4,	17,	12,	4,
           9,	17,	15,	9,
           14,	27,	25,	13,
           17,	28,	27,	19,
           20,	33,	29,	22,
           24,	36,	32,	18,
           29,	42,	35,	27), start=2012, frequency=4)
print(TS11)
autoplot(TS11)

# part a
library(fpp2) # Forecasting: Principles and Practice Ver.2

# Centered Moving Average 4 quarters
TS11_CMA <- ma(TS11, order = 4, centre = T)
print(TS11_CMA)

# part b
# plot.ts plots several time series on a common plot.
ts.plot(TS11_CMA, TS11) # CMA values smooths out the ts by removing seasonal effects

# alt method autoplot
autoplot(TS11, series="Sales") + 
  autolayer(TS11_CMA, series="CMA") + # add alayer, plots 5-MA
  xlab("Year") + ylab("Sales")

# part c
# Seasonal Indexes (Decomposition)
Decomp_TS11 <- decompose(TS11, type = "multiplicative" )
autoplot(Decomp_TS11)
print(Decomp_TS11$seasonal)

# largest seasonal increase in "quarter 2".
# largest seasonal decrease in "quarter 4".
# This is also reasonable because of decreased boating in the fall and winter.
# Q4: Oct-Nov-Dec

# Case Study (Forecasting Lost Sales  Carlson Department Store) ----

## Q1  ----
# An estimate of sales for Carlson Department Store had there been no hurricane.

library(readxl)
carlson <- read_excel("carlsonsales.xlsx")
glimpse(carlson)
head(carlson)

# Building time series
# Start 9th month (September 2015)
carl_ts <- ts(carlson$Sales, start= c(2015, 9), frequency = 12)
print(carl_ts)
autoplot(carl_ts)

# Decomposition
carl_decomp <- decompose(carl_ts, type = "multiplicative" )
autoplot(carl_decomp)
print(carl_decomp)

# Calculating De-seasonalized Sales
carl_des_sales <- carl_ts/carl_decomp$seasonal
carlson <- cbind(carlson, carl_des_sales)

# Using the Deseasonalized Time Series to Identify Trend
# Simple Linear Regression
carl_slr <- lm(carl_des_sales ~ Period, data = carlson)
summary(carl_slr)

# Forecasting without irregularity or hurricane (4 month closed, Sep - Dec) 
carl_des_forecast <- predict(carl_slr, list(Period=c(49:52)))
carl_forecast <- carl_des_forecast * carl_decomp$seasonal[1:4]
print(carl_forecast)


## Q2  ----

library(readxl)
county_df_full <- read_excel("countysales.xlsx")
glimpse(county_df_full)
head(county_df_full)
# Note: County Excel File has 52 month with Actual values of Sep - Dec of yr 5)

county_df <- county_df_full[c(1:48),] # we need a comparable data set 

# Building time series
# Start 9th month (Sep) of 2015
county_ts <- ts(county_df$Sales, start= c(2015, 9), frequency = 12)
print(county_ts)
autoplot(county_ts)

# Decomposition
county_decomp <- decompose(county_ts, type = "multiplicative" )
autoplot(county_decomp)
print(county_decomp)

# Calculating De-seasonalized Sales
county_des_sales <- county_ts/county_decomp$seasonal
county_df <- cbind(county_df, county_des_sales)
county_df$Period <- c(1:48)

# Using  Deseasonalized Time Series to Identify Trend
# Simple Linear Regression
county_slr <- lm(county_des_sales ~ Period, data = county_df)
summary(county_slr)

# Forecasting based on the slr (4 month closed, Sep - Dec)
county_des_forecast <- predict(county_slr, list(Period=c(49:52)))
county_forecast <- county_des_forecast * county_decomp$seasonal[1:4]
print(county_forecast)


## Q3  -----

# Lost Sales Calculation
# Comparison: county forecast vs. county actual sales

# Actual Values (4 month closed, Sep - Dec)
county_actual <- county_df_full$Sales[c(49:52)]
print(county_actual)

Lost_Sales <- data.frame(county_actual, county_forecast)
Lost_Sales$Ratio_A_F <- Lost_Sales$county_actual/Lost_Sales$county_forecast
# These ratios are also called lift factor

mean(Lost_Sales$Ratio_A_F)
# For the 4-month total, actual sales exceeded the forecast by around 30%.
# Explanation: people had to replace personal property damaged by the storm.

Lost_Sales$Carl_F <- carl_forecast
Lost_Sales$Carl_LS <- Lost_Sales$Carl_F * Lost_Sales$Ratio_A_F
sum(Lost_Sales$Carl_LS)

# Carlson suffered a business interruption claim of $15,867,000.






