# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504/Ch_15")

# Q1 ----
# read excel file
library(readxl)
Q1 <- read_excel("showtime.xlsx")
head(Q1)
summary(Q1)

# change column names
colnames(Q1) <- c( "revenue", "tv", "news")

# Part a, simple regression
slr1a <- lm(revenue ~ tv, data = Q1)
summary(slr1a)
coef(slr1a)

# Part b, multiple regression
slr1b <- lm(revenue ~ tv + news, data = Q1)
summary(slr1b)
coef(slr1b)

# part c, coefficients for tv are not the same in part a and b
  # part a: tv coef. shows change in revenue due to a unit change in tv ads
  # part b: tv coef. shows change in revenue due to a unit change in tv ads with news ads held constant.

# predict tv = 3600 and news = 1600
round(
  predict(slr1b, data.frame(tv=3.6, news = 1.6))
  *1000,
  0)

# Q2 ----
# read excel file
library(readxl)
Q2 <- read_excel("Ships.xlsx")
head(Q2)

# change column names
colnames(Q2) <- c( "ship", "overall", "shore", "food")

# Part a, simple regression
slr2a <- lm(overall ~ shore, data = Q2)
summary(slr2a)
coef(slr2a)

# Part b, multiple regression
slr2b <- lm(overall ~ shore + food, data = Q2)
summary(slr2b)
coef(slr2b)

# predict shore 80 and food = 90
round(
  predict(slr2b, data.frame(shore=80, food = 90)),
  2)

# Q3 ----
# read excel file
library(readxl)
Q3 <- read_excel("Exer2.xlsx")
head(Q3)

# multiple regression
slr3 <- lm(Y ~ X1 + X2, data = Q3)
slr3$coefficients # coefficients

# part a, b R2 and Adj. R2
summary(slr3) 
summary(slr3)$r.squared # R2
summary(slr3)$adj.r.squared # Adjusted R2

# part c, R2> 0.7 regression equation explain a large amount of the variability in the data

# Q4 ----
library(readxl)
Q4 <- read_excel("PitchingMLB.xlsx")
head(Q4)

# change some column names
colnames(Q4)[6] <- "SO_IP"
colnames(Q4)[7] <- "HR_IP"
colnames(Q4)[8] <- "R_IP"

head(Q4)

# part a: multiple regression
slr4a <- lm(R_IP ~ SO_IP + HR_IP, data = Q4)
slr4a$coefficients # coefficients

# part b: R2 and Adj. R2
summary(slr4a) 
summary(slr4a)$r.squared # R2
summary(slr4a)$adj.r.squared # Adjusted R2

# The fit is not bad, because it explains 52.82% of the variability

# part c: multiple regression
slr4c <- lm(ERA ~ SO_IP + HR_IP, data = Q4)
slr4c$coefficients # coefficients

# part a: R2 and Adj. R2
summary(slr4c) 
summary(slr4c)$r.squared # R2
summary(slr4c)$adj.r.squared # Adjusted R2

# The fit is not bad, because it explains 59.00% of the variability

# Q5 ----
# read excel file
library(readxl)
Q5 <- read_excel("showtime_1.xlsx")
head(Q5)
# change column names
colnames(Q5) <- c( "revenue", "tv", "news")

# Part a, F test in multiple regression 
slr5 <- lm(revenue ~ tv + news, data = Q5)
summary(slr5)
summary(slr5)$fstatistic[1] # F-statistic

# The overall model is  significant

# Part b, c: t test test in multiple regression 
summary(slr5)
summary(slr5)$coefficients[ , 3] # Returning t-value
summary(slr5)$coefficients[ , 4] # Returning p-value

# Q6 ----
library(readxl)
Q6 <- read_excel("PitchingMLB_1.xlsx")
head(Q6)

# change some column names
colnames(Q6)[6] <- "SO_IP"
colnames(Q6)[7] <- "HR_IP"
colnames(Q6)[8] <- "R_IP"

head(Q6)

# multiple regression
slr6 <- lm(R_IP ~ SO_IP + HR_IP, data = Q6)
slr6$coefficients # coefficients

# part a, b: F and t Test
summary(slr6) 

# Q7 ----
# read excel file
library(readxl)
Q7 <- read_excel("showtime_2.xlsx")
head(Q7)
# change column names
colnames(Q7) <- c( "revenue", "tv", "news")
head(Q7)

# Part a: prediction
slr7 <- lm(revenue ~ tv + news, data = Q7)
summary(slr7)

predict(slr7, data.frame( tv = 3.5, news = 1.8))

# Part b, prediction interval
predict(slr7, data.frame( tv = 3.5, news = 1.8), interval = "prediction", level = 0.95)

# Q8 ----
library(readxl)
Q8 <- read_excel("autoresale.xlsx")
head(Q8)

# Part a: prediction
slr8 <- lm(Price ~ Mileage + Age, data = Q8)
summary(slr8)

predict(slr8, data.frame(Mileage = 40000, Age = 4))

# Part b, confidence and prediction interval
predict(slr8, data.frame(Mileage = 40000, Age = 4), interval = "confidence", level = 0.95)
predict(slr8, data.frame(Mileage = 40000, Age = 4), interval = "prediction", level = 0.95)

# Q9 ----
# Read Excel file in R
library(readxl)
repair_1 <- read_excel("repair_1.xlsx")
head(repair_1)

#change column names
colnames(repair_1) <- c("time", "months", "type", "person")

# Re-level categorical data
repair_1$type <- relevel(factor(repair_1$type), ref = "Mechanical")
repair_1$person <- relevel(factor(repair_1$person), ref = "Bob Jones")

# Regression
repair_1_model <- lm(time ~ months + type + person, data = repair_1)

#coefficients, statistics, and sig levels
summary(repair_1_model) 

# The addition of Person is not statistically significant.
# Person is highly correlated with months since last service

# If we have one continuous variable and one categorical variable,
# it is impossible to calculate the correlation between them. 
# we can use regression to come up with r-sq value and take the square root of that r-sq value.
repair_2_model <- lm(months ~ person, data = repair_1)
sqrt(summary(repair_2_model)$r.squared) # very close to 0.7

# Q10 ----
# Read Excel file in R
library(readxl)
stroke <- read_excel("Stroke.xlsx")
head(stroke)

# part a and b Regression
stroke_model <- lm(Risk ~ Age + Pressure + Smoker, data = stroke)
summary(stroke_model)
coefficients(stroke_model)

# part c predict
predict(stroke_model, data.frame(Age = 68, Pressure = 175, Smoker = "Yes"))

# The physician would recommend that Art quit smoking (b3) and begin bp treatment (b2)

# Q11 ----
# Read Excel file in R
library(readxl)
auto <- read_excel("Auto2.xlsx")
head(auto)

# Curb weight is the weight of the vehicle including a full tank of fuel and all standard equipment.

# change col names
colnames(auto) <- c( "car", "price", "weight", "hp", "speed")

# part a Regression
auto_model <- lm(speed ~ price + hp, data = auto)
summary(auto_model)
coefficients(auto_model)

# residual analysis
plot(auto_model, which = 3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Residual analysis 
auto$predicted <- fitted(auto_model)
auto$residuals <- residuals(auto_model)
auto$std_residuals <- rstandard(auto_model)

auto %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# residual plot support the assumption about error
# There are no std residuals above +2 or below -2

# Q12 ----
# read excel file
library(readxl)
Q12 <- read_excel("showtime_3.xlsx")
head(Q12)

# change column names
colnames(Q12) <- c( "revenue", "tv", "news")
head(Q12)

# Part a: regression
slr12 <- lm(revenue ~ tv + news, data = Q12)
summary(slr12)
coefficients(slr12)

# part b std residual plot
# residual analysis
plot(slr12, which = 3)
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Residual analysis 
Q12$predicted <- fitted(slr12)
Q12$residuals <- residuals(slr12)
Q12$std_residuals <- rstandard(slr12)

Q12 %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE)  # we observe no pattern

# few observations, difficult to determine error assumptions violated.
# For instance, an argument could be made that there does not appear to be any pattern in the plot
# alternatively an argument could be made that there is a curvilinear pattern in the plot.

# none of the standard residuals are less than -2 or greater than 2 (no outlier)

# Q13 ----
# read excel file
library(readxl)
NBA <- read_excel("NBAStats.xlsx")
head(NBA)

colnames(NBA) <- c( "team", "win", "fg", "tp", "ft", "rboff", "rbdef")
head(NBA)

# Part a: regression
nba_model_1 <- lm(win ~ fg, data = NBA)
summary(nba_model_1)
coefficients(nba_model_1)

# At the 0.05 level of there is a significant relationship

# part b
# An increase in the percentage of field goals made will increase the percentage of games won.

# part c
nba_model_2 <- lm(win ~ fg + tp + ft + rboff + rbdef, data = NBA)
summary(nba_model_2)
round (coefficients(nba_model_2), 4)

# ft (the percentage of the free throws made) is not sig. drop

nba_model_3 <- lm(win ~ fg + tp + rboff + rbdef, data = NBA)
summary(nba_model_3)
round (coefficients(nba_model_3), 4)

# part e prediction
predict(nba_model_3, data.frame(fg = 45, tp = 35, rboff = 12, rbdef = 30))

# case (NASCAR) ----
library(readxl)
nascar <- read_excel("nascar.xlsx")
head(nascar)

#Part 1
plot(nascar)
round(cor(nascar[,-1]),2)

# alt method
library(PerformanceAnalytics)
chart.Correlation(nascar[,-1],
                  histogram = TRUE)
#The variable most highly correlated with Winnings ($) is the number of top-ten finishes.
# simple regression winnings and top10

# change col names
colnames(nascar)[5] = "Top5"
colnames(nascar)[6] = "Top10"
colnames(nascar)[7] = "Winnings"

nascar_model_1<- lm(Winnings ~ Top10, data = nascar)
summary(nascar_model_1)

#Part 2
nascar_model_2 <- lm(Winnings ~ Poles + Wins + Top5 + Top10, data = nascar)
summary(nascar_model_2)

# t-test: The only significant variable is Top 10, with a p-value of .0015.
# Adj. R2 is 0.797, model w/ only Top 10 had adj. R2 of 0.8001
# Adding more IVs added little to the model’s ability to explain variation in Winnings.

#Part 3
#Top 2-5
nascar$Top2_5 <- nascar$Top5 - nascar$Wins

#Top 6-10
nascar$Top6_10 <- nascar$Top10 - nascar$Top5
nascar_model_3 <- lm(Winnings ~ Poles + Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_3)

# T test: only IV not significant is Poles, with a p-value of .9047.
# we get better results because we greatly reduced the multicollinearity
# Multicollinearity is reduced by replacing Top 5 with Top 2–5 and replacing Top 10 with Top 6–10.
# Correlation Matrix below provides evidence of this.
round(cor(nascar[, -c(1,5,6)]),2) # excluding driver, top5, and top10

library(PerformanceAnalytics)
chart.Correlation(nascar[, -c(1,5,6)],
                  histogram = TRUE)

# part 4
# Keep Wins, Top2-5, and Top6-10
nascar_model_4 <- lm(Winnings ~ Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_4)

# Wins: one unit increase in wins, all other IV constant, increases winnings by $204,735.
# Top 2–5: one unit increase in Top 2–5 finish, all other IV constant, increases winnings by $186,778.
# Top 6–10: one unit increase in Top 6–10 finish, all other IV constant, increases winnings by $116,189.

# case (Lakeland) ----

#Part a) logit(Return) = b0 + b1(Program) + b2(GPA)

#Part b) What is the interpretation of E( y) when x2 = 0?
# For a given GPA, it's the prob. that a student who not attended orientation will return for sophomore year.

#Part c) Logistic Regression (Estimated Logit is the reg. equation 15.37)

library(readxl)
lakeland <- read_excel("Lakeland.xlsx")
head(lakeland)

# logistic regression
lakeland_model <- glm(Return ~ Program + GPA, data = lakeland, family = 'binomial')
coefficients(lakeland_model)

# coefficients represent changes in log-odds (ln(odds) of dv and by themselves are hard to interpret
# we need to convert them to odds ratio using exp() function 
exp(lakeland_model$coefficients[-1]) # we don't need intercept

# odds-ratios interpretation for IVs
# categorical IV (e.g. program): the odds of program attendees returning vs non-attendees
# program = 4.76, odds of an attendee returning are 4.76x a non-attendee

# continuous IV (e.g. GPA): the odds of returning increased for every additional unit of GPA
# GPA = 12.66, for every unit increase in GPA, odds of returning as a sophomore is 12.67x higher

# Part d) Overall Significance
summary(lakeland_model)

# alt method
library(car)
Anova(lakeland_model, test = "LR") # likelihood ratio test

# R does not report overall model significance (beyond scope)
# the significance of  model is tested by "Analysis of Deviance"
# It could be obtained by a Chi-Squared test (below code).
pchisq(lakeland_model$null.deviance - lakeland_model$deviance, 
       lakeland_model$df.null - lakeland_model$df.residual, lower.tail = FALSE)

#Part e) Each IV Significance
summary(model)

#Part f) predict
predict(lakeland_model, data.frame(GPA = 2.5, Program = 0), type = "response")
predict(lakeland_model, data.frame(GPA = 2.5, Program = 1), type = "response")

#Part g) Odds Ratios
lakeland_model$coefficients
exp(lakeland_model$coefficients[-1])
#odds of continuing for students who attended orient 4.7624 x greater those not.

#Part h) Recommendation
# We recommend making the orientation program required. 
# odds of continuing are much higher for students who have attended the orientation program.
