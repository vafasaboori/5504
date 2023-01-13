# Clear the environment
rm(list = ls())

# Read Excel file in R
library(readxl)
armands <- read_excel("armands.xlsx")
summary(armands)

# Examine Correlation between variables (Multiple R in Excel)
cor(armands$Population, armands$Sales)

# Simple linear regression
armands_slr <- lm(Sales ~ Population, data = armands)
summary(armands_slr) # coefficients

# Plotting data and regression line
library(tidyverse)
armands %>% ggplot(aes(x = Population, y = Sales)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf interval (gray)

              
              
              