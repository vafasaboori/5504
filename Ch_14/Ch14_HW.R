# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504/Ch_14")

# Q1 ----
Q1 <- data.frame(x = c(2, 6, 9, 13, 20), y = c(7, 18, 9, 26, 23))

# Part a, Scatter Plot
plot (Q1$x, Q1$y)
abline(lm(Q1$y ~ Q1$x))

# Using ggplot (recommended)
library(tidyverse)
Q1 %>% ggplot(aes(x, y)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # ab-line with conf. interval (gray)

# Part b, regression
slr1 <- lm(y ~ x, data = Q1)
summary(slr1)
coef(slr1)

predict(slr1, data.frame(x=6))

# Q2 ----
# Read Excel file in R
library(readxl)
Q2 <- read_excel("data14-05.xlsx")
summary(Q2)

# Rename one variables (too long)
colnames(Q2)[1] <- "speed"

# rename all variables
colnames(Q2) <- c( "speed", "defective")

# alt method
names(Q2) <- c( "speed", "defective")

# Part a, Scatter Plot
plot (Q2$speed, Q2$defective)
abline(lm(Q2$defective ~ Q2$speed))

# Using ggplot (recommended)
library(tidyverse)
Q2 %>% ggplot(aes(speed, defective)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # ab-line with conf. interval (gray)

# part b, plot shows a negative relationship

# part c, regression
slr2 <- lm(defective ~ speed, data = Q2)
summary(slr2)
predict(slr2, data.frame(speed=25))

# Q3 ----
library(readxl)
Q3 <- read_excel("landscape.xlsx")
summary(Q3)

# Rename one variables (too long)
colnames(Q3) <- c("value", "exp")

# Part a, Scatter Plot
plot (Q3$value, Q3$exp)
abline(lm(Q3$exp ~ Q3$value))

# Using ggplot (recommended)
library(tidyverse)
Q3 %>% ggplot(aes(value, exp)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # ab-line with conf. interval (gray)

# part b, plot shows a positive relationship

# part c, regression
slr3 <- lm(exp ~ value, data = Q3)
summary(slr3)
coefficients(slr3)

# part d, variable cost
coefficients(slr3)[2] # should be multiplied by 1000

# part e predict
predict(slr3, data.frame(value=575)) # should be multiplied by 1000

# Q4 ----
library(readxl)
Q4 <- read_excel("data14-13.xlsx")
summary(Q4)

# Rename one variables (too long)
colnames(Q4) <- c( "distance", "absent")

# Part a, Scatter Plot
plot (Q4$distance, Q4$absent)
abline(lm(Q4$absent ~ Q4$distance))

# Using ggplot (recommended)
library(tidyverse)
Q4 %>% ggplot(aes(distance, absent)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # ab-line with conf. interval (gray)

# part b, plot shows a negative relationship

# part c, regression
slr4 <- lm(absent ~ distance, data = Q4)
summary(slr4)

# part d predict
predict(slr4, data.frame(distance=5)) # should be multiplied by 1000

# part e round
round (predict(slr4, data.frame(distance=5)),0) # should be multiplied by 1000

# Q5 ----
Q5 <- data.frame(x=c(2, 6, 9, 13, 20), y=c(7, 18, 9, 26, 23))

# part a, SSR SSE SST
slr5 <- lm(y ~ x, data = Q5)

library("car") # Companion to Applied Regression
anova <- anova(slr5)

#extract values
ssr <- anova$`Sum Sq`[1]
sse <- anova$`Sum Sq`[2]
sst <- ssr + sse

# R-squared
summary (slr5)$r.squared 


# Q6 ----
Q6 <- data.frame(x = c (400, 450, 550, 600, 700, 750), 
                 y = c(4000, 5000, 5400, 5900, 6400, 7000))

# part a, regression
slr6 <- lm(y ~ x, data = Q6)
summary(slr6)

# part b, variable cost
coefficients(slr6)[2]

# part c, R-squared
summary (slr6)$r.squared 

# part d, predict
predict(slr6, data.frame(x=500))


# Q7 ----
Q7 <- data.frame(x = c(2, 6, 9, 13, 20),
                 y = c(7, 18, 9, 26, 23))
slr7 <- lm(y ~ x, data = Q7)

# part a, standard error of estimate
summary(slr7)$sigma

#part b and c sig. test
summary(slr7)


# Q8 ----
Q8 <- data.frame(x <- c(400, 450, 550, 600, 700, 750),
                 y <- c(4000, 5000, 5400, 5900, 6400, 7000))
slr8 <- lm(y ~ x, data = Q8)
summary(slr8)

# Anova Table
library(car)
anova(slr8)

# Q9 ----
Q9 <- data.frame(x = c(3, 12, 6, 20, 14),
                 y = c(55, 40, 55, 10, 15))
slr9 <- lm(y ~ x, data = Q9)
summary(slr9)

predict(slr9, data.frame(x=8), interval = "confidence")
predict(slr9, data.frame(x=8), interval = "prediction")

# Q10 ----
Q10 <- data.frame(x = c(22, 27, 32, 48, 65, 85, 120),
                 y = c(9.6, 9.6, 10.1, 11.1, 13.5, 17.7, 25.5))
slr10 <- lm(y ~ x, data = Q10)
summary(slr10)
predict(slr10, data.frame(x=52.5), interval = "confidence")
predict(slr10, data.frame(x=52.5), interval = "prediction")

# Q11 ----
library(readxl)
Q11 <- read_excel("BusinessTravel.xlsx")
summary(Q11)

# rename all variables
colnames(Q11) <- c("city", "Rate", "Ent")

slr11 <- lm(Ent ~ Rate, data = Q11)
summary(slr11)

predict(slr11, data.frame(Rate=89), interval = "confidence")
predict(slr11, data.frame(Rate=128), interval = "prediction")

# Q13 ----
Q13 <- data.frame(x = c(6, 11, 15, 18, 20),
                  y = c(6, 8, 12, 20, 30))
slr13 <- lm(y ~ x, data = Q13)

#part a
coefficients(slr13)

# part b residuals
slr13$residuals

# Residual plot against y hat
plot(slr13, which = 1)

# Residual plot against x and y-hat
Q13$predicted <- fitted(slr13)
Q13$residuals <- residuals(slr13)

library(tidyverse)

# Residual plot against y hat 
Q13 %>% ggplot(aes(predicted , residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Residual plot against x
Q13 %>% ggplot(aes(x , residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Std Residual plot against y hat
Q13$std_residuals <- rstandard(slr13)
Q13 %>% ggplot(aes(x = predicted, y =std_residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern


# Q14 ----

Q14 <- data.frame (x = c(4, 5, 7, 8, 10, 12, 12, 22),
                  y = c(12, 14, 16, 15, 18, 20, 24, 19))

slr14 <- lm(y ~ x, data = Q14)
summary(slr14)

# part b std res
Q14$std_rediduals <- rstandard(slr14) # last observation is an outlier

# part c hat values
Q14$hat <- hatvalues(slr14)  # last observation is greater than 6/8=0.75
qplot (Q14$x, Q14$hat) +
  geom_hline(yintercept=0.75, linetype="dashed", color = "red")

# cook's distance
plot(slr14, which = 5) #last observation greater than 0.5

# Q15 ----
library(readxl)
Q15 <- read_excel("checkout.xlsx")
summary(Q15)

# rename all variables
colnames(Q15) <- c( "arrival", "shopping")

# part a and b, scatter plot
Q15 %>% ggplot(aes(x = arrival, y = shopping)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

# part c regression
slr15 <- lm(shopping ~ arrival, data = Q15)
summary(slr15)

# part d residual analysis
plot (slr15, which = 1)
plot (slr15, which = 5) # obs 32 is an outlier

# using std residuals
Q15$std_residuals <- rstandard(slr15) #obs 32 is an outlier

# Using Leverage (Hat Values)
Q15$hat <- hatvalues(slr15) 
qplot (Q15$arrival, Q15$hat) +
  geom_hline(yintercept=0.1875, linetype="dashed", color = "red") #obs 32 is an outlier

# Removing Last Observation
Q15_Rev <- Q15[-32,]

# scatter plot
Q15_Rev %>% ggplot(aes(x = arrival, y = shopping)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

slr15_Rev <- lm(shopping ~ arrival, data = Q15_Rev)
summary(slr15_Rev)

# part d residual analysis
plot (slr15_Rev, which = 1)
plot (slr15_Rev, which = 5) #no outlier

# using std residuals
Q15_Rev$std_residuals <- rstandard(slr15_Rev) # no outlier


# Q16 ----
library(readxl)
Q16 <- read_excel("camry.xlsx")
summary(Q16)

# rename all variables
colnames(Q16) <- c( "miles", "price")

# part a scatter plot
Q16 %>% ggplot(aes(x = miles, y = price)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

# part c regression
slr16 <- lm( price ~ miles, data = Q16)
summary(slr16)
predict(slr16, data.frame(miles=60))

# Buckeye Case ----
library(readxl)
buck <- read_excel("buckeyecreek.xlsx")

# Descriptive Statistics
summary(buck)

# rename all variables
colnames(buck) <- c( "zip", "population", "pass")

# part a scatter plot
buck %>% ggplot(aes(x = population, y = pass)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

# Regression
buckeye_slr <- lm(pass ~ population, data = buck)
summary (buckeye_slr)
anova(buckeye_slr)

# Residuals Analysis
plot(buckeye_slr, which =1) # Fan (Not Good!)
plot(buckeye_slr, which =5) # No outliers

# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph
