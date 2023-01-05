# ggplot2 is one of the core members of the tidyverse
install.packages("tidyverse")
library(tidyverse)

#mpg data frame found in ggplot2
library(ggplot2)
ggplot2::mpg

# To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis:
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()
