# Clear the environment
rm(list = ls())

library(readr)
admission <- read_csv("admission.csv")
View(admission)

# First, we convert rank to a factor to indicate that rank should be treated as a categorical variable.
admission$rank <- factor(admission$rank)

# The code below estimates a logistic regression model using the glm (generalized linear model) function. 
admission_logit <- glm(admit ~ gre + gpa + rank, data = admission, family = "binomial")
summary(admission_logit)

# Both gre and gpa are statistically significant, as are the three terms for rank. 
# Coefficients give the change in the log odds of the outcome for a unit increase in the IV.

# For every one unit change in gre, the log odds of admission  increases by 0.002.
# For a one unit increase in gpa, the log odds of being admitted  increases by 0.804.
# The indicator variables for rank have a slightly different interpretation. 
# For example:
# attending institution with rank of 2, versus rank of 1, changes the log odds of admission by -0.675.

# You can also exponentiate the coefficients and interpret them as odds-ratios. 
# R will do this computation for you. To get the exponentiated coefficients, 

# odds ratios only
exp(coef(admission_logit))

admission$fitted_Prob <- predict(admission_logit, type = "response")
# It can also be helpful to use graphs of predicted probabilities
ggplot(admission, aes(x = gre, y = fitted_Prob)) + 
  geom_smooth(aes(colour = rank),
                                                                                                                      size = 1)
