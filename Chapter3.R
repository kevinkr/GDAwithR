#Chapter 3
library(ggplot2)
data(btw2009, package = "flexclust")
btw2009 <- within(btw2009, Linke2 <- 100*LINKE2/valid2)
ggplot(btw2009, aes(Linke2)) + geom_bar(binwidth = 1, 
       fill = "mediumpurple") + ylab("") +
       xlab("Percentage voter support for Die Linke in 2009")

#histogram
data(galton, package="UsingR")
ht <- "height (in)"
par(mfrow=c(1,2), las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  hist(child, xlab="Height", main="Children", col="green")
  hist(parent, xlab=ht, main="Parents", col="blue")})

#histogram with binwidths of 0.1
par(mfrow=c(1,2), mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  MASS::truehist(child, h=0.1)
  MASS::truehist(parent, h=0.1)})

#histograms one above the other using the same scale
c1 <- ggplot(galton, aes(child)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") + 
  geom_vline(xintercept=median(galton$child),
             col="red")
p1 <- ggplot(galton, aes(parent)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") +
  geom_vline(xintercept=median(galton$parent),
             col="red")
grid.arrange(c1, p1)

#Histograms and overlaid density estimates of the heights of father and sons
#Densities can only be succesfully overlaid when the histogram scales are
#densities instead of frequencies, which is why y=..density.. is needed
data(father.son, package="UsingR")
c2 <- ggplot(father.son, aes(sheight)) + 
  geom_histogram(aes(y = ..density.., ), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.2,fill="blue") + xlim(58, 80) + ylim(0, 0.16) +
  xlab("ht (inches)") + ylab("") + ggtitle("Sons") 
p2 <- ggplot(father.son, aes(fheight)) + 
  geom_histogram(aes(y = ..density..), colour="black", fill="white", binwidth=1) +
  geom_density(alpha=.2,fill="red") + xlim(58, 80) + ylim(0, 0.16) +
  xlab("ht (inches)") + ylab("") +
  ggtitle("Fathers")
grid.arrange(c2, p2, nrow = 1)

#normality with Q-Q plots
with(father.son, {
  qqnorm(sheight, main="Sons", xlab="",
         ylab="", pch=16, ylim=c(55,80))
  qqline(sheight)
  qqnorm(fheight, main="Fathers", xlab="",
         ylab="", pch=16, ylim=c(55,80))
  qqline(fheight)})

#Scottish hill races
par(mfrow=c(1,1), mar=c(3.1, 4.1, 1.1, 2.1))
with(MASS::hills,
     boxplot(time, horizontal=TRUE, pch=16, ylim=c(0, 220)))

#Boston data set
#Examine median values of ownership homes by area
ggplot(MASS::Boston, aes(medv)) + geom_histogram(binwidth = 1) + ylab("") +
  xlab("Median housing value (thousands of dollars)")

#Histogram of all variables using melt
#melt creates new dataset with all the data in one variable, BostonValues
#second variable called BostonVars defines which of the original variables
#a value comes from
library(tidyr)
B2 <- gather(MASS::Boston, BosVars, BosValues, crim:medv)
ggplot(B2, aes(BosValues)) + geom_histogram() + xlab("") +
  ylab("") + facet_wrap(~ BosVars, scales = "free")

#comparing histogram plots
library(MASS)
par(mfrow=c(1,2))
with(MASS::Boston, hist(ptratio))
with(MASS::Boston, truehist(ptratio))

#boxplot
par(mfrow=c(1,1))
boxplot(MASS::Boston$medv, pch=16)

#jittered boxplot
stripchart(MASS::Boston$medv, method="jitter", pch=16)

#stem and leaf plots
stem(MASS::Boston$medv)

#Average shifted histograms
library(ash)
plot(ash1(bin1(MASS::Boston$medv, nbin=50)), type="l")

#density estimates with a rugplot
d1 <- density(MASS::Boston$medv)
plot(d1, ylim=c(0,0.1))
rug(MASS::Boston$medv)
lines(density(MASS::Boston$medv, d1$bw/2), col="green")
lines(density(MASS::Boston$medv, d1$bw/5), col="blue")

#How long is a movie
data(movies, package = "ggplot2movies")
ggplot(movies, aes(length)) + geom_bar() + ylab("")

#one outlier distorts the plot
#look for it this way
s1 <- filter(movies, length > 2000)
print(s1[,c("length","title")], row.names=FALSE)

#boxplot of film length
ggplot(movies, aes("var", length)) + geom_boxplot() +
  xlab("")  + scale_x_discrete(breaks=NULL) + coord_flip()

#histogram set with movie length of 3 hours
ggplot(movies, aes(x = length)) +  xlim(0,180) +
  geom_histogram(binwidth=1)  +
  xlab("Movie lengths in minutes") + ylab("")

#Distributions by subgroup
btw2009 <- within(btw2009, Bundesland <- state)
btw2009 <- within(btw2009, levels(Bundesland) <- c("BW", "BY", "BE", "BB",
                                                   "HB", "HH", "HE", "MV", "NI", "NW","RP", "SL", "SN", "ST", "SH", "TH"))
ggplot(btw2009, aes(Bundesland, Linke2)) + geom_boxplot(varwidth=TRUE) + ylab("")

