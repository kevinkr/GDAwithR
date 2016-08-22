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

#3. Student survey
#(a) draw histogram of student heights and overlay a density estimate. Is there
#evidence of bimodality?
data(survey)
summary(survey)
hist(survey$Height)
c2 <- ggplot(survey, aes(Height)) + 
  geom_histogram(aes(y = ..density.., ), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.4,fill="blue") + 
  xlab("ht (inches)") + ylab("") + ggtitle("Students") 
c2
#(b) Experiment with different binwidths and bandwidths for the density estimates
c2 <- ggplot(survey, aes(Height)) + 
  geom_histogram(aes(y = ..density.., ), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.4,fill="blue") + stat_density(adjust = 2, alpha=0.5) +
  xlab("ht (inches)") + ylab("") + ggtitle("Students") 
c2

c2 <- ggplot(survey, aes(Height)) + 
  geom_histogram(aes(y = ..density.., ), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.4,fill="blue") + stat_density(adjust = .25, alpha=0.5) +
  xlab("ht (inches)") + ylab("") + ggtitle("Students") 
c2

#(c) Compare male and female heights using separate density estimates that are 
#common scaled and aligned ith one another.
#histograms one above the other using the same scale
f.survey <- subset(survey, Sex=='Female')
m.survey <- subset(survey, Sex=='Male')

c1 <- ggplot(f.survey, aes(Height)) + 
  geom_histogram(aes(y = ..density.., ), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.4,fill="red") +
  xlim(150, 205) + ylim(0, 0.15)
  geom_vline(xintercept=median(f.survey$Height),
             col="red") + ggtitle("Female")
  
p1 <- ggplot(m.survey, aes(Height)) + 
  geom_histogram(aes(y = ..density.., ), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.4,fill="blue") +
  xlim(150, 205) + ylim(0, 0.15)
  geom_vline(xintercept=median(m.survey$Height),
             col="red") + ggtitle("Male")
grid.arrange(c1, p1)

##
#4
##

##
#5. Zuni educational funding
#
library(lawstat)
data(zuni)
summary(zuni)
hist(zuni$Revenue, nclass=200)
nrow(zuni)
boxplot(zuni$Revenue)
