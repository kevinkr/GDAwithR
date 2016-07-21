library(ggplot2)
library(gridExtra)
library(ggthemes)
library(dplyr)
library(GGally)
library(vcd)
library(extracat)

#http://www.gradaanwr.net/content/01-setting-the-scene/
library(ggplot2)
library(ggthemes)
data(SpeedSki, package = "GDAdata")
ggplot(SpeedSki, aes(x=Speed, fill=Sex)) + xlim(160, 220) +
  geom_histogram(binwidth=2.5) + xlab("Speed (km/hr)") +
  facet_wrap(~Sex, ncol=1) + ylab("") +
  theme(legend.position="none")

#original by sex
ggplot(SpeedSki, aes(Speed, fill=Sex)) +
  geom_histogram(binwidth=2.5) + xlab("Speed (km/hr)") +
  ylab("") + facet_grid(Sex~Event) +
  theme(legend.position="none")

#by nation
ggplot(SpeedSki, aes(Speed, fill=Nation)) +
  geom_histogram(binwidth=2.5) + xlab("Speed (km/hr)") +
  ylab("") + facet_grid(Nation~Event) +
  theme(legend.position="none")

#iris
ggplot(iris, aes(Petal.Length)) + geom_histogram()

ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) +
  geom_point() + theme(legend.position="bottom") +
  scale_colour_colorblind()

library(gridExtra)
ucba <- as.data.frame(UCBAdmissions)
a <- ggplot(ucba, aes(Dept)) + geom_bar(aes(weight=Freq))
b <- ggplot(ucba, aes(Gender)) + geom_bar(aes(weight=Freq))
c <- ggplot(ucba, aes(Admit)) + geom_bar(aes(weight=Freq))
grid.arrange(a, b, c, nrow=1, widths=c(7,3,3))

library(vcd)
ucb <- data.frame(UCBAdmissions)
ucb <- within(ucb, Accept <- 
                factor(Admit, levels=c("Rejected", "Admitted")))
doubledecker(xtabs(Freq~ Dept + Gender + Accept, data = ucb),
             gp = gpar(fill = c("grey90", "steelblue")))

#Pima Indians diabetes dataset
#stacked 3x2 histogram
data(Pima.tr2, package="MASS")
h1 <- ggplot(Pima.tr2, aes(glu)) + geom_histogram()
h2 <- ggplot(Pima.tr2, aes(bp)) + geom_histogram()
h3 <- ggplot(Pima.tr2, aes(skin)) + geom_histogram()
h4 <- ggplot(Pima.tr2, aes(bmi)) + geom_histogram()
h5 <- ggplot(Pima.tr2, aes(ped)) + geom_histogram()
h6 <- ggplot(Pima.tr2, aes(age)) + geom_histogram()
grid.arrange(h1, h2, h3, h4, h5, h6, nrow=2)

#boxplot
library(dplyr)
PimaV <- select(Pima.tr2, glu:age)
par(mar=c(3.1, 4.1, 1.1, 2.1))
boxplot(scale(PimaV), pch=16, outcol="red")

#scatterplot
library(GGally)
ggpairs(PimaV, diag=list(continuous='density'),
        axisLabels='show')

#Exercise 1
#How would you describe this histogram of sepal width?
ggplot(iris, aes(Sepal.Width)) +
    geom_histogram(binwidth = 0.1)

#Exercise 2
#Summarise what this barchart shows
ggplot(Pima.tr2, aes(type)) + geom_bar()

#Exercise 3
ggplot(Pima.tr2, aes(age,npreg)) + geom_point()

#Ex 4
sol <- data.frame(michelson, package = "MASS")
ggplot(michelson, aes(Speed)) + geom_histogram()
ggplot(michelson, aes(Speed,Run)) + geom_point()

ggplot(michelson, aes(Speed, Run, color=Expt)) +
  geom_point() + theme(legend.position="bottom") +
  scale_colour_colorblind()
