library(ggplot2)
library(gridExtra)
library(ggthemes)
library(dplyr)
library(GGally)
library(vcd)
library(extracat)
library(MASS)
#Chapter 3 Exercises
#1 1. Galaxies
data(galaxies)
summary(galaxies)
str(galaxies)
names(galaxies)
#The dataset is called galaxies in the package MASS.
#(a) Draw a histogram, a boxplot, and a density estimate of the data. What information can you get
#from each plot?
hist(galaxies)
boxplot(galaxies)
rug(galaxies)
#(b) Experiment with different binwidths for the histogram and different bandwidths for the density
#estimates. What choices do you think are best for conveying the information in the data?
hist(galaxies, 
     nclass = 100,
     main = "Galaxies", 
     xlab = "Velocity", 
     ylab = "",
     col = "light blue")

#2. Boston housing
#How would you describe the distribution of the 14 variables from this plot
library(tidyr)
B2 <- gather(MASS::Boston, BosVars, BosValues, crim:medv)
ggplot(B2, aes(BosValues)) + geom_histogram() + xlab("") +
  ylab("") + facet_wrap(~ BosVars, scales = "free")
#age, dis, lstat, medv, nox rm look like even distributions
boxplot(MASS::Boston$black)
boxplot(MASS::Boston$chas)
