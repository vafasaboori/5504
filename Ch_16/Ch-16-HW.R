# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504/Ch_16")

# Q1 (16-01 Algo) ----

# build data frame
Q1 <- data.frame(
  x = c(22,	24,	26,	30,	35,	40),
  y = c(12, 21,	33,	35,	40,	36))

# part a
Q1_model_a <- lm(y ~ x, data = Q1) 
coef(Q1_model_a)

# part b
summary(Q1_model_a) # summary
summary(Q1_model_a)$r.squared # extract r-squared
summary(Q1_model_a)$fstatistic # extract F Statistic
summary(Q1_model_a)$coefficients[2,4] # extract p-value of t-test (same as F in slr)
anova(Q1_model_a)[1,5] # extract p-value of F-test directly
# The relationship between x and y is not significant; the fit is weak

# part c
library(tidyverse)
Q1 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm")
# scatter diagram suggest a form  y = b0 + b1(x) + b2(x^2)
# Because the points are arranged approximately on the parabola

# part d
Q1_model_b <- lm(y ~ poly(x, degree = 2, raw = T), data = Q1)
coef(Q1_model_b)

# let's visualize the quadratic fit
Q1 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # much better fit

# part e
summary(Q1_model_b)
summary(Q1_model_b)$r.squared # extract r-squared
summary(Q1_model_b)$fstatistic # extract F Statistic
summary(Q1_model_a)$coefficients[2,4] # extract p-value of 
anova(Q1_model_b)[1,5] # extract p-value of F-test directly
# curvilinear relationship is significant and provides a very good fit

# part f
predict(Q1_model_b, data.frame(x=25))

# Q2 (16-02 Algo) ----

# build data frame
Q2 <- data.frame(
  x = c(7, 31, 21, 18, 22),
  y = c(9, 27, 20, 16, 20))

# part a
Q2_model_a <- lm(y ~ x, data = Q2) 
summary(Q2_model_a)
coef(Q2_model_a) # coefficients

summary(Q2_model_a)$sigma # s or Residual standard error (RSE)
# RSE square root of the mean squared error (MSE)
# RSE is a measure of variability of the residuals 
# RSE provides an indication of how well the regression model fits the data
# A smaller (comparative) RSE indicates a better fit of the model to the data (smaller residuals)
anova(Q2_model_a)

summary(Q2_model_a)$r.squared # extract r-squared
summary(Q2_model_a)$adj.r.squared # extract r-squared

anova(Q2_model_a) # anova table
# strong relationship with 99.04313% of variation in y explained by x

# let's visualize this slr
Q2 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm") # good fit

# part b
Q2_model_b <- lm(y ~ poly(x, degree = 2, raw = T), data = Q2)
summary(Q2_model_b)
coef(Q2_model_b) # coefficients

# let's visualize the quadratic relationship
Q2 %>% ggplot(aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # slightly better fit

summary(Q2_model_b)$sigma # s or Residual standard error (RSE)
summary(Q2_model_b)$r.squared # extract r-squared
summary(Q2_model_b)$adj.r.squared # extract r-squared

anova(Q2_model_b) # anova table
# strong relationship with 99.13428% of variation in y explained by x

# part c
predict(Q2_model_b, data.frame(x=18))

# Q3 (16-04 Algo) ----

# build data frame
Q3 <- data.frame(
  flow = c(1221, 1277, 1356, 1233, 1328, 1369),
  speed = c(30, 35, 40, 30, 45, 50))

# part a
Q3_model <- lm(flow ~ speed, data = Q3) 
summary(Q3_model)
coef(Q3_model) # coefficients

summary(Q3_model)$sigma # s or Residual standard error (RSE)
summary(Q3_model)$r.squared # extract r-squared
summary(Q3_model)$adj.r.squared # extract r-squared

anova(Q3_model) # anova table (sig. slr model)

# visualization of slr
Q3 %>% ggplot(aes(x = speed, y = flow)) + 
  geom_point() +
  geom_smooth(method = "lm") # good fit

# Q4 (16-05 Algo) ----

# build data frame
Q4 <- data.frame(
  flow = c(1256, 1330, 1226, 1336, 1349, 1124),
  speed = c(35, 45, 35, 50, 50, 30))

# part a
Q4_model <- lm(flow ~ poly(speed, degree = 2, raw = T), data = Q4)
coef(Q4_model) # coefficients

# let's visualize the quadratic relationship
Q3 %>% ggplot(aes(x = speed, y = flow)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # visually better fit

summary(Q4_model)$r.squared # r-sq went up from 0.85 to 0.99 (better fit)

# part b
summary(Q4_model)$fstatistic # extract F Statistic
anova(Q4_model)[1,5] # extract p-value of F-test directly
# curvilinear relationship is significant and provides a very good fit

# part c
predict(Q4_model, data.frame(speed=39))

# Q5 (16-06 Algo) ----

# build data frame
Q5 <- data.frame(
  fac = c(6, 9, 13, 15, 21, 27),
  dist = c(1.6, 0.99, 0.62, 0.48, 0.41, 0.38))

# part b
plot(Q5)

# part b
# slr not appropriate, the relationship appears to be curvilinear

# Let's visualize linear fit
library(tidyverse)
Q5 %>% ggplot(aes(x=fac, y=dist)) + 
  geom_point() +
  geom_smooth(method = "lm",
              se=FALSE)

# Let's visualize quadratic fit
library(tidyverse)
Q5 %>% ggplot(aes(x=fac, y=dist)) + 
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se=FALSE)

# port c
# quadratic
Q5_model_a <- lm(dist ~ poly(fac, degree = 2, raw = T), data = Q5)
coef(Q5_model_a) # coefficients
summary(Q5_model_a)$r.squared # r-squared

# reciprocal
Q5_model_b <- lm(dist ~ I(1/fac), data = Q5)
# I() function is used to indicate that the independent variable x should be transformed into 1/x.

coef(Q5_model_b) # coefficients
summary(Q5_model_b)$r.squared # r-squared

# visualization of reciprocal transformation
library(tidyverse)
Q5 %>% ggplot(aes(x=fac, y=dist)) + 
  geom_point() +
  geom_smooth(method = "lm",
            formula = y ~ I(1/x),
            se=FALSE) # slightly better fit

# let's compare r-squard values
summary(Q5_model_a)$r.squared # r-squared 97%
summary(Q5_model_b)$r.squared # r-squared 98%

# let's visually compare linear, quadratic, and reciprocal all together
Q5 %>% ggplot(aes(x=fac, y=dist)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se=FALSE, col = "tomato") +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se=FALSE, col = "violet") +
  geom_smooth(method = "lm",
              formula = y ~ I(1/x),
              se=FALSE, col = "turquoise")
  
# R offers about 657 color names. You can read all of them using
colors()

# Q6 (16.08 Algo)----

# read excel file
library(readxl)
classic <- read_excel("ClassicCars.xlsx")
glimpse(classic)
head(classic)

# change column names
colnames(classic)[5] = "Price"
head(classic)

# part a, scatter plot 
classic %>% ggplot(aes(x=Rating, y=Price)) + 
  geom_point() 
# slr model does not appear to be appropriate.

# part b
classic_model_a <- lm(Price ~ poly(Rating, degree = 2, raw = T),
                      data = classic)

coefficients(classic_model_a) # coefficients
summary(classic_model_a)$r.squared # r-squared
summary(classic_model_a)$fstatistic # F
anova(classic_model_a) # anova table
anova(classic_model_a)
anova(classic_model_a)[1,5] # p-value for F

# Visualization
classic %>% ggplot(aes(x=Rating, y=Price)) + 
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2)) 
  
# part c
classic_model_b <- lm(log(Price) ~ log(Rating), data = classic)
coefficients(classic_model_b) # coefficients
summary(classic_model_b)$r.squared # r-squared (better model)
summary(classic_model_b)$fstatistic # F
anova(classic_model_a)[1,5] # p-value for F almost 0

# part d, The model in part (c) is preferred because it provides a better fit.
# Q7 (16.10 Algo) ----

# part a
# MSR = SSR / DFR = (SST - SSE) / 1 = (1480 - 590) / 1 = 890 / 1 = 890
# DFE = n - p - 1 = 24 - 1 - 1 = 22
# MSE = SSE / DFE = 590 / 22 = 26.8
# DFR = p = 1
# F = MSR / MSE = 890 / 26.8 = 32.93
# The p-value can be found using an F-distribution table with DFR and DFE degrees of freedom 
# at sig 0.01, the critical value for this test would be approximately 6.39.
# F-statistic (32.93) is much larger than the critical value, reject the null hypothesis

# part b
# MSR = (SST - SSE) / DFR = (1480 - 150) / 3 = 1330 / 3 = 443.33
# DFE = n - p -1 = 20
# MSE = SSE / DFE = 150 / 20 = 7.5
# F = MSR / MSE = 443.33 / 7.5 = 59.1
# at sig 0.01, the critical value for this test would be approximately 4.64
# F-statistic (59.1) is much larger than the critical value, reject the null hypothesis

# Q8(16.11 Algo) ----

# part a
# SSE = SST - SSR = 1805 - 1752 = 53
# MSR = SSR / degrees of freedom (df) for regression = 1752 / 4 = 438
# MSE = SSE / df for error = 53 / (30-4) = 2.038
# The F test statistic can be calculated as MSR / MSE = 350.4 / 2.038 = 214.59
# For 4 and 26 degrees of freedom, the critical F at sig 0.05 is 3.17
# F (214.59) is much larger, conclude that there is a significant relationship

# pat b
# SSE = SST - SSR = 1805 - 1705 = 100.
# MSE = SSE / degrees of freedom for error = 100 / (30 - 3) = 4.55
# F = MSR / MSE, which is 1705 / 4.55 = 376.35 sig at 0.05

# Q9 (16.14 Algo) ----
# read excel file
library(readxl)
stroke_1 <- read_excel("stroke_1.xlsx")
glimpse(stroke_1)
head(stroke_1)

# Rename column
colnames(stroke_1)[3] = "BP"

# visualization
stroke_1 %>% ggplot(aes(x= Age, y= Risk,
                      color = BP)) +
  geom_point() +
  geom_smooth(method = lm)

# alt method
stroke_1$BP_cat <- cut(stroke_1$BP, breaks = c(0, 120, 130, 140, 500),
                     labels = c("Normal", "Elevated", "Stage 1 Hyp", "Stage 2 Hyp") )

stroke_1 %>% ggplot(aes(x= Age, y= Risk,
                      col = BP_cat)) +
  geom_point() +
  geom_smooth(method = "lm")

# part a 
stroke_1_model_a <- lm(Risk ~ Age + BP, data = stroke_1)
summary(stroke_1_model_a)
anova(stroke_1_model_a)


# part b add two IVs: 1-interaction between age and BP, 2-smoker
stroke_1_model_b <- lm(Risk ~ Age + BP + Smoker + Age*BP, data = stroke_1)
summary(stroke_1_model_b)
anova(stroke_1_model_b)

# part c, test if the addition of the interaction and the smoker contribute significantly

library(car) # companion to applied regression
anova(stroke_1_model_a)
anova(stroke_1_model_b)
# At this point you can read SSE(reduced), SSE(full), calculate MSE, etc.

# alt method (more efficient)
# first build a new interaction variable
stroke_1$Age.BP <- stroke_1$Age * stroke_1$BP
stroke_1_model_c <- lm(Risk ~ Age + BP + Smoker + Age.BP, data = stroke_1)
summary(stroke_1_model_c)

# install.packages("car")
library(car)
Added_IV <- c("Smoker", "Age.BP")
linearHypothesis(stroke_1_model_c, Added_IV)
# Addition of the two terms is not significant
# linearHypothesis computes a F statistic for comparing a full model and a reduced model

# Refresher on DOE ----
# First let's review the concepts (slides and excel)

# Secton 9-1 DOE (GMAT) ----

library(readxl)
gmat <- read_excel("gmatstudy.xlsx")
head(gmat)

# This format is not useful in R, we need to reshape
library(reshape2)
gmat_a <- melt(gmat, id=c("Preparation"))

# change column names
colnames(gmat_a) <- c("Prep", "Major", "GMAT")

# convert prep to factor
gmat_a$Prep <- as.factor(gmat_a$Prep)
levels(gmat_a$Prep)

# convert major to factor
gmat_a$Major <- as.factor(gmat_a$Major)
levels(gmat_a$Major)

gmat_anova <- aov(GMAT ~ Major*Prep, data = gmat_a)
summary(gmat_anova)

# DOE Using Regression
gmat_model <- lm(GMAT ~ Prep*Major, data = gmat_a) # both variables and interaction
anova(gmat_model)
#Preparation: not sig. / Major: sig. / Interaction: not sig.

# Section 9-2 DOE using Regression (Chemitech) ----

# First let's review the concept (slides and excel)

library(readxl)
chem <- read_excel("chemitech.xlsx")
head(chem)

# This format is not useful in R, we need to reshape
library(reshape2)
chem_a <- melt(chem)

# Rename "Method A" to "A"
chem_a$method <- c(rep("A",5), rep("B",5), rep("C", 5))

# convert "method" to factor
chem_a$method <- as.factor(chem_a$method)
levels(chem_a$method)

colnames(chem_a)[2] <- "units"
chem_model <- lm(units ~ method, data = chem_a)
anova(chem_model)

# Q10 (16.20 Algo) ----

# create data set
jacobs <- data.frame(manuf = c(rep("A",4), rep("B",4), rep("C", 4)),
            time = c(16, 22, 20, 18, 27, 25, 30, 26, 16, 15, 19, 18))

# convert to factor (categorical var)
jacobs$manuf <- as.factor(jacobs$manuf)
levels(jacobs$manuf)

# In case you want to relevel
# jacobs$manuf <- relevel(jacobs$manuf, ref = "B")
# levels(jacobs$manuf)

# method 1 (without dummy variables)
jacobs_model <- lm(time ~ manuf, data = jacobs)
anova(jacobs_model)

# method 2 (create dummy variables)
jacob_dummies <- as.data.frame(model.matrix(~ manuf , jacobs))
colnames(jacob_dummies) = c("I", "D1", "D2")

# combine dummies with original data
jacobs_1 <- cbind (jacobs, jacob_dummies)

jacobs_1_model <- lm(time ~ D1 + D2 , data = jacobs_1)
summary(jacobs_1_model)
anova(jacobs_1_model)

# null hyp: b1 = b2 = 0
summary(jacobs_1_model)$fstatistic # F (sig at 0.01) reject null

# Q11 (16.20 Algo) ----

# build data frame
auto <- data.frame( 
  analyzer = factor(rep(c("computer", "electronic"), times = 3)),
  car = factor(c(rep("compact", 2), rep("inter", 2), rep("full", 2))),
                      time = c(46, 38, 52, 41, 64, 47))# create data set
  
# method 1 (without dummy variables)
auto_model <- lm(time ~ analyzer + car, data = auto)
anova(auto_model)

# method 2 (create dummy variables)

# check levels
levels(auto$analyzer)
levels(auto$car)

# for "car" "we need to relevel
auto$car <- factor(auto$car, levels = c("compact", "inter", "full"))
levels(auto$car)

# create dummy variables
auto_dummies <- as.data.frame(model.matrix(~ analyzer + car , auto))
colnames(auto_dummies) = c("I", "X1", "X2", "X3")

# combine dummies with original data
auto_1 <- cbind (auto, auto_dummies)

auto_1_model <- lm(time ~ X1 + X2 + X3, data = auto_1)
summary(auto_1_model)
anova(auto_1_model)

# Q12 (16.24 Algo) ----
library(readxl)
closing_1 <- read_excel("closingprice_1.xlsx")
glimpse(closing_1)

# Part a. Define the independent variable Period
closing_1$Period <- c(1:20)

# rename columns
names(closing_1)[2] <- "Price"

closing_1_model <- lm(Price ~ Period, data = closing_1)
summary(closing_1_model)

# Part b. DW Test for Auto-correlation
library(lmtest)
dwtest(closing_1_model)
# Remember that null here is "we do not have autocorrelation". When reject null: Unhappy!

# Critical Value Method, dL = 1.2, dU = 1.41, d < dL significant positive autocorrelation.

# Notes:
# Autocorrelation occurs when the values of a time series are not independent of one another.
# Autocorrelation can lead to incorrect results in your analysis.
# To deal with autocorrelation, you can difference the time series.
# You can difference the time series by subtracting each value from the value that comes before it. 
# This helps to remove the autocorrelation, making the values more independent of one another.
# Alternatively, you can use methods that account for autocorrelation, such as ARIMA or SARIMA (beyond scope).
# You may want to consider using cross-sectional analysis or panel data analysis (beyond scope).

# Q13 (16.26 Algo) ----

library(readxl)
bonds <- read_excel("CorporateBonds.xlsx")
glimpse(bonds)

# Change col. names
colnames(bonds) = c("tic", "yrs", "yld")

# part a. scatter plot
bonds %>% ggplot(aes(x= yrs, y= yld)) +
  geom_point() +
  geom_smooth(method = lm)

#  A simple linear regression model does not appear to be appropriate.

# part b. quadratic regression
bonds_model_1 <- lm(yld ~ poly(yrs, degree = 2, raw = T), data = bonds)

coef(bonds_model_1) # coefficients

summary(bonds_model_1)
summary(bonds_model_1)$r.squared 
summary(bonds_model_1)$fstatistic

anova(bonds_model_1)
anova(bonds_model_1)[1,5] # extract p-value of F-test directly

# part c. log regression
bonds_model_b <- lm(yld ~ log(yrs), data = bonds)
coefficients(bonds_model_b) # coefficients
summary(bonds_model_b)$r.squared # r-squared (better model)
summary(bonds_model_b)$fstatistic # F
anova(bonds_model_b)[1,5] # p-value for 

# Log model provides a better fit than second order, r-sq higher

# Q14 (16.28) ----
library(readxl)
audit <- read_excel("audit.xlsx")
glimpse(audit)

# part a  regression
audit_model_a <- lm(Delay ~ Industry + Public + Quality + Finished, data = audit)
coef(audit_model_a) # coefficients

# part b
summary(audit_model_a)
summary(audit_model_a)$r.squared # not a good fit, non-linear

# part c, scatter plot
audit %>% ggplot(aes(Finished, Delay)) + 
  geom_point()
  
# part d

# add fifith IV Finished^2
audit$Finished2 <- audit$Finished^2

# regression
audit_model_b <- lm(Delay ~ Industry + Public + Quality + Finished + Finished2, data = audit)
summary(audit_model_b)

# best subset
# olsrr package
library(olsrr) # ordinary least squares regression models

# Forward Selection using p-values
ols_step_forward_p(audit_model_b, penter=.05, details = F)

# Backward Elimination using p-values
ols_step_backward_p(audit_model_b, prem=.05, details = F)

# Stepwise regression using p-values
ols_step_both_p(audit_model_b, pent=.05, prem=.05, details = F)

# Best subsets regression
ols_step_best_subset(audit_model_b, details = T)
# We look for high r-squared and low AIC, SBIC, and SBC

# best model with 2 IV: Finished and Finished-Squared
# best model with 3 IV: Industry, Finished and Finished-Squared
# Using the best subset regression procedure, 4 IVs in the highest adjusted  model
# R^2 for model 4 is: 54.37%

# Q15 (16.30 Algo) ----

library(readxl)
audit_1 <- read_excel("AuditDelays.xlsx")
glimpse(audit_1)

# part a, regression
audit_1_model <- lm(Delay ~ Industry + Quality, data = audit)
coef(audit_1_model) # coefficients

# part b, DW Test for Auto-correlation
library(lmtest)
dwtest(audit_1_model)

# At the .05 level of significance, dL = 1.39 and dU = 1.60  
# Because the DW is dL < 1.4261 < dU, The test is inconclusive

# Q16 (16.33 Algo) ----

library(readxl)
fuel <- read_excel("fueleconomy2019.xlsx")
glimpse(fuel)

# change column names
colnames(fuel)[4] = "MPG"

# convert Class to factor (categorical var)
fuel$Class <- as.factor(fuel$Class)
levels(fuel$Class)

# for "Class" "we need to relevel
fuel$Class <- factor(fuel$Class, levels = c("Standard SUV 4WD",
                                            "Two Seaters", 
                                            "Minicompact Cars",
                                            "Subcompact Cars",
                                            "Compact Cars",
                                            "Midsize Cars",
                                            "Large Cars",
                                            "Small Station Wagons",
                                            "Small SUV 2WD",
                                            "Small SUV 4WD"))
levels(fuel$Class) # levels ok

# create dummies
fuel_dummies <- as.data.frame(model.matrix(~ Class , fuel))
colnames(fuel_dummies) = c("I", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9")

# combine dummies with original data
fuel_1 <- cbind (fuel, fuel_dummies)

fuel_1_model <- lm(MPG ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 , data = fuel_1)
coefficients(fuel_1_model)
summary(fuel_1_model)
anova(fuel_1_model)
anova(Q4_model)[1,5] # extract p-value of F-test directly

# Case Study Piedmont Wine ----

library(readxl)
wine <- read_excel("WineRatings.xlsx")
glimpse(wine)

# Q1 1.	A table showing the number of wines and average price for each rating

# R for Excel Users Book: https://rstudio-conf-2020.github.io/r-for-excel/
# https://rstudio-conf-2020.github.io/r-for-excel/pivot-tables.html#group_by-summarize

library(dplyr)
wine %>%
  group_by(Rating) %>%
  summarise(
    n = n(),
    avg_price = mean(Price)
    )
# None of the wines reviewed received a Not Recommended rating 
# only one wine was rated Mediocre.
# Overall, 85% of the wines received a Very Good or Outstanading rating. 
# With  exception of 1 wine rated Mediocre, price  is greater for wines that are rated higher.

# Q2 Scatter diagram
plot(wine$Price, wine$Score)

# Alt Method
wine %>% ggplot(aes(x = Price, y = Score)) + 
  geom_point() 

# Q3 linear regression
wine_model_1 <- lm(Score ~ Price, data = wine)
summary(wine_model_1)

# visualization
wine %>% ggplot(aes(x = Price, y = Score)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Q4 Quadratic Regression
wine_model_2 <- lm(Score ~ poly(Price, degree=2, raw=T), data = wine)
summary(wine_model_2)

# Q5 Model 2 is better fit, R-Sq (adj) = 51.35% compared to R-Sq = 40.62% for slr

# let's plot them on the same coordiane
wine %>% ggplot(aes(x = Price, y = Score)) + 
  geom_point() +
  geom_smooth(method = "lm",
              se=FALSE, col = "tomato") +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se=FALSE, col = "violet")

# Q6 log model
wine$lnPrice <- log(wine$Price)
wine_model_3 <- lm(Score ~ lnPrice, data = wine)
summary(wine_model_3)

# Log model gives slightly better fit than the second-order model, with R-Sq (adj) = 57.2%.

# Q7, Spending more for a bottle of wine will, in general, provide a better wine. 
library(dplyr)
wine_1 <- filter(wine, Price <=30)
plot(wine_1$Price, wine_1$Score)

# Alt method 
wine_1 %>% 
  ggplot(aes(Price, Score)) + #dataset is piped into ggplot, just need to set coordinates
  geom_point(size=1, alpha=.5) + #data points with size 1 and semi-transparent
  geom_smooth() # no specific relationship between price and score for this group of wines. 

# You could probably pick a wine at random from this group!

library("ggplot2")

wine %>% 
  ggplot(aes(x = Price, y = Score, 
             color = Rating)) + #different ratings in different color
  geom_point(size=2, alpha=.5) +
  geom_smooth(method = "lm", se= FALSE) #data points with size 3 and semi transparent

wine %>% 
  ggplot(aes(x = Price, y = Score)) +
  geom_point(size=1, alpha=.5) + # data points with size 1 and semi-transparent
  geom_smooth(method = "lm") +
  facet_wrap(~ Rating, scales = "free") # breaks data into different groups and plots them
#  scales = "free" allows both the x and y axes to be on different scales for each facet


# In Class Practice (Case Study: LPGA) ----

# Include the following in your report
# Descriptive statistics and the sample correlation coefficients for the data (interpret)
# SLR model with best IV (Interpret r-squared)
# Investigate  other IVs using all methods including stepwise regression procedure (interpret)
# What IVs combination give you the best result?
# Prepare a report that summarizes your analysis, conclusions, and recommendations.


