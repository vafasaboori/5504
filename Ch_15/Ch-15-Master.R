# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504/Ch_15")

# Section 1 Multiple Regression Coefficients ----

# Read Excel file in R
library(readxl)
butler <- read_excel("butler.xlsx")
summary(butler)

# multiple regression
butler_model <- lm(Time ~ Miles + Deliveries, data = butler)
butler_model$coefficients # coefficients

# Section 2 R2 and Adjusted R2 ----
summary(butler_model) 
summary(butler_model)$r.squared # R2
summary(butler_model)$adj.r.squared # Adjusted R2

# alt method rsq package
library(rsq)
rsq(butler_model, adj = FALSE) # R2
rsq(butler_model, adj = TRUE) # adjusted RR2

# Section 3 Test of Significance ----
summary(butler_model) # t Test and F Test

# alt method
library(car)
Anova(butler_model) # Separate F Tests

# alt method
drop1(butler_model) #  SSR for each IV
drop1(butler_model, test = "F") # Separate F Tests
drop1(butler_model, test = "Chisq") # Separate Chi-Squared Tests

# Section 4 In Class Practice (Q15-7 Satisfaction) ----

#Read Excel File
library(readxl)
sat <- read_excel("satisfaction.xlsx")
summary(sat)

# Change col names
colnames(sat) <- c("global" , "job", "pay", "org")

# part a
sat_model <- lm(global ~ job + pay + org, data = sat)
summary(sat_model)

# Part b
predict(sat_model, data.frame(job = 72, pay = 54, org = 53))

# Section 5 In Class Practice (Q15-9 House Prices) ----

#Read Excel File
library(readxl)
house <- read_excel("springhouses.xlsx")
summary(house)

# Change col names
colnames(house) <- c("price" , "bath", "sqft", "beds")

# part a
plot(house)

# alt method
pairs(house, lower.panel = panel.smooth) # same plot

# alt method
library(GGally)
ggscatmat(house, alpha = 0.5) # more visually appealing, r values

# part b
house_model <- lm(price ~ bath + sqft + beds, data = house)
summary(house_model)
# we observe that "bath" is not significant

# part c drop "bath"
house_model_rev <- lm(price ~ sqft + beds, data = house)
summary(house_model_rev)

# part d prediction
predict(house_model_rev, data.frame(sqft = 2650, beds = 4))

# Section 6 Multicollinearity ----

# Correlation plot (matrix)
cor(mtcars)
round(cor(mtcars), 2)

# alt method
library(corrplot)
corrplot(cor(mtcars))
# added visualization
corrplot(cor(mtcars), method = "number")

# alt method
library(GGally)
ggcorr(mtcars, label = TRUE)
?ggcorr # help file

# alt method
library(PerformanceAnalytics)
chart.Correlation(mtcars,
                  histogram = TRUE)

# Section 7 In Class Practice (Q15-25 Auto-Resale) ----

# Read Excel file in R
library(readxl)
auto <- read_excel("autoresale.xlsx")
summary(auto)

# part a, multiple regression
auto_model <- lm(Price ~ Mileage + Age, data = auto)
summary(auto_model)

# part b, Multicollinearity
plot(auto)
cor(auto)

# alt method
library(GGally)
ggcorr(auto, label = TRUE)
# Mileage and Age Cor Coeff is 0.67 which is less than 0.7 (OK)

# Section 8 Prediction and Confidence Interval NFL ----
# Read Excel file in R
library(readxl)
nfl <- read_excel("nfl2011.xlsx")
summary(nfl)

#change column names
colnames(nfl) <- c("team", "offense", "defense", "win")

# Regression
nfl_model <- lm(win ~ offense + defense, data = nfl)
summary(nfl_model)

#Coefficient and CI for Coefficients
coef(nfl_model)
confint(nfl_model, level = .95)

#Part a
predict(nfl_model, data.frame(offense=225, defense=300), interval = "confidence", level = .95)
predict(nfl_model, data.frame(offense=225, defense=300), interval = "prediction", level = .95)

# Section 9 Categorical Independent Variables ----
# Read Excel file in R
library(readxl)
repair <- read_excel("repair.xlsx")
summary(repair)

#change column names
colnames(repair) <- c("time", "months", "type", "person")

# Regression
repair_model <- lm(time ~ type + months, data = repair)
summary(repair_model) # coeff for type negative
# R creates dummy automatically and chooses baseline alphabetic or numerical
# Default baseline is Electrical type=0 (alphabetically)
# In this case we want to change baseline for Mechanical to type=0

repair$type <- relevel(factor(repair$type), ref = "mechanical")
repair_model <- lm(time ~ type + months, data = repair)
summary(repair_model)

# Section 10 In Class Practice (Q15-37 Fridge) ----
# Read Excel file in R
library(readxl)
fridge <- read_excel("refrigeratorsizeprice.xlsx")
summary(fridge)

#change column names
colnames(fridge) <- c("model", "door", "cubft", "price")

# part a 
fridge_slm <- lm(price ~ cubft, data = fridge)
summary (fridge_slm)

# part b
# p-value = .0002, significant relationship between list price and cubic feet.

# part c dummy variable (in R we don't need to develope a dummy variable!)
# Regression
fridge_model <- lm(price ~ door + cubft, data = fridge)
summary(fridge_model)
#	p-value = .7508 > α = .05
# there is no sig. relationship between list price and thru-the-door feature.

# Here No is door=0, and yes is door=1
# If we want to change the baseline to Yes is door=0
fridge$door <- relevel(factor(fridge$door), ref = "Yes")

# then run the model again
fridge_model <- lm(price ~ door + cubft, data = fridge)
summary(fridge_model)

# Section 11 Residual Analysis ----

# Read Excel file in R
library(readxl)
butler <- read_excel("butler.xlsx")
summary(butler)

# multiple regression
butler_model <- lm(Time ~ Miles + Deliveries, data = butler)
butler_model$coefficients # coefficients

# residual analysis
plot(butler_model, which = 5)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph
# Section 12 Logistic Regression ----

# Read Excel file in R
library(readxl)
simmons <- read_excel("Simmons.xlsx")
summary(simmons)

# plot logistic regression with only one IV
library(tidyverse)
simmons %>% ggplot(aes(Spending, Coupon)) +
  geom_point(size = 3, alpha = 0.7) + # scatter plot
  geom_smooth(method = "glm", method.args = list(family = "binomial"))
#glm is general linear model
#method.args = list(family = "binomial") specifies the logistic regression model.

# Regression analysis
library(stats)
simmons_model <- glm(Coupon ~ Card + Spending, data = simmons, family ='binomial')
summary(simmons_model)
# Family binomial indicates that it's a logistic regression problem
# If we have categorical IV, it should be converted to a factor before being used in the model.
# You can do this using the factor function in R.

#let's focus on the coefficients
coefficients(simmons_model)

# coefficients represent  change in  log odds of the dependent variable
# coefficients represent changes in log-odds (ln(odds) of dv and by themselves are hard to interpret
# we need to convert them to odds ratio using exp() function 
exp(simmons_model$coefficients[-1]) # we don't need intercept

# odds-ratios interpretation for IVs
# categorical IV (e.g. card): the odds of cardholders using coupon vs non-cardholders
  # card = 3, odds of a cardholder using the coupon are 3x a non-cardholder
# continuous IV (e.g. spending): the odds of using coupon increased for every additional 1k
  # spending = 1.4, for every 1k increase in spending, odds of using coupon is 40% higher

# To predict individual probabilities we use this:
predict(simmons_model, data.frame(Card = 1, Spending = 4.2), type = "response")
# type = 'response' specifies that you want predicted probabilities
# probability of a cardholder with 4.2k spending is %59.6

predict(simmons_model, data.frame(Card = 1, Spending = 4.2))
# without "type = 'response'" you will get the log odds, aka logits (logarithms of odds ratios). 
# logit of a cardholder with 4.2k spending is %38.7

# To convert logit "ln(p/1-p)" to probability use the inverse logit function
p <- 1 / (1 + exp(-0.387261))
print (p)
# logit value of %38 corresponds to probability value of of %59.6
