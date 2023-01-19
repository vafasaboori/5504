# Clear the environment
rm(list = ls())
setwd("~/Documents/5504/Ch_14")

# Section 1 ----

# Read Excel file in R
library(readxl)
armands <- read_excel("armands.xlsx")
summary(armands)

# Examine Correlation between variables (Multiple R in Excel)
cor(armands$Population, armands$Sales)

# Simple linear regression
armands_slr <- lm(Sales ~ Population, data = armands)
armands_slr$coefficients # coefficients

# Plotting data and regression line
library(tidyverse)
armands %>% ggplot(aes(x = Population, y = Sales)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

# Section 2 ----

# R Squared Value
summary(armands_slr)$r.squared

# Adjusted R Squared Value
summary(armands_slr)$adj.r.squared

# Section 3 ----

# Complete regression analysis
summary(armands_slr)

# Confidence and Prediction Interval
predict (armands_slr, data.frame(Population = 10), interval = "confidence", conf.level = 0.95)
predict (armands_slr, data.frame(Population = 10), interval = "prediction", conf.level = 0.95)

# Section 4 ----

# Residual analysis (method 1)
armands$predicted <- fitted(armands_slr)
armands$residuals <- residuals(armands_slr)
armands$std_residuals <- rstandard(armands_slr)

# Residual plot against x
armands %>% ggplot(aes(x = Population, y = residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Residual plot against y-hat
armands %>% ggplot(aes(x = predicted, y = residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Standard residual plot against x
armands %>% ggplot(aes(x = Population, y =std_residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# QQ Plot 
armands %>% ggplot(aes(sample = std_residuals)) +
  geom_qq() + 
  geom_qq_line()

# Section 5 ----

# Calculating Leverage (Hat Values)
armands$hat <- hatvalues(armands_slr) #none greater than 6/10=0.6
qplot (armands$Population, armands$hat) +
  geom_hline(yintercept=0.6, linetype="dashed", color = "red")

# Residual analysis (method 2)
plot(armands_slr, which = 1) # can produce 5 plots
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

plot(armands_slr, which = 4) #none greater than 0.5
armands$cook <- cooks.distance(armands_slr)
# Cook's distance is a combination of leverage and residual values
# The higher the leverage and residuals, the higher the Cook’s distance.
# Investigate any point over 0.5, values over 1.0 are influential

plot(armands_slr, which = 5) #none greater than 0.5
# in this plot (leverage ~ std) we are looking for values lying outside dashed line

