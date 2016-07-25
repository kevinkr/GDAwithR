library(ggplot2)
library(gridExtra)
library(ggthemes)
library(dplyr)
library(GGally)
library(vcd)
library(extracat)
library('RCurl')
library('foreign')

#http://www.gradaanwr.net/content/01-setting-the-scene/
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

#Exercise 4
#Estimating the speed of light
#There are 100 estimates of the speed of light made by Michelson in 1879, composed
#of 5 groups of 20 experiments each (dataset michelson in the MASS package).
#(a) What plot would you draw for showing the distribution of all the values
#together? What conclusions would you draw?

#plot  Experiments
plot(michelson$Expt, michelson$Speed, xlab="Experiment number", main="Speed of light data")
ggplot(data=michelson, aes(Expt, Speed)) + geom_point()

#plot runs
plot(michelson$Run, michelson$Speed, xlab="Run no.", main="Speed of light data")


fm <- aov(michelson$Speed ~ michelson$Run + michelson$Expt)		# Run and Expt are factors
summary(fm)
#Result shows that Run is not an important factor.
fm0 <- update( fm, . ~ . - michelson$Run)
anova(fm0, fm)

#(b) What plots might be useful for comparing the estimates from the 5 dif

sol <- data.frame(michelson, package = "MASS")
ggplot(michelson, aes(Speed)) + geom_histogram()
ggplot(michelson, aes(Speed,Run)) + geom_point()

ggplot(michelson, aes(Speed, Run, color=Expt)) +
  geom_point() + theme(legend.position="bottom") +
  scale_colour_colorblind()

with(michelson, boxplot(Speed ~ Expt)) 

# I can add colour and labels. I can also save the results to an object.

michelson.bp = with(michelson, boxplot(Speed ~ Expt, xlab="Experiment", las=1, 
                                       ylab="Speed of Light - 299,000 m/s", 
                                       main="Michelson-Morley Experiments",
                                       col="slateblue1")) 
# The current estimate of the speed of light, on this scale, is 734.5
# Add a horizontal line to highlight this value.
abline(h=734.5, lwd=2,col="purple")  #Add modern speed of light


#Exercise 5
#The liner Titanic sank on its maiden voyage in 1912 with great loss of life. The
#dataset is provided in R as a table. Convert this table into a data frame using
#data.frame(Titanic).
data(Titanic)
titanic <- as.data.frame(Titanic)

require(graphics)
mosaicplot(Titanic, main = "Survival on the Titanic")

ggplot(data=titanic) + geom_bar(aes(x=Pclass))
ggplot(data=titanic) + geom_bar(aes(x=Sex))

#Read in full train data from titanic
titanic <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header = TRUE, stringsAsFactors = FALSE)

qplot(Age, data=titanic, colour=as.factor(Pclass))
qplot(Age, Fare, data=titanic, colour=as.factor(Pclass))

qplot(Age, Fare, data=titanic, colour=as.factor(Pclass), facets=~Sex+Embarked)
qplot(Age, Fare, data=titanic, colour=as.factor(Pclass), facets=Sex~Embarked)

ggplot(titanic, aes(x=Age, y=Fare)) + geom_point()
ggplot(titanic, aes(x=Age, y=Fare)) + geom_hex()
ggplot(titanic, aes(x=Age, y=Fare)) + geom_smooth()
ggplot(titanic, aes(x=Age, y=Fare)) + geom_line()
ggplot(titanic, aes(x=Age, y=Fare)) + geom_density2d()


g <- ggplot(titanic, aes(Age, Fare))
g + geom_hex() + geom_smooth()
g + geom_point() + geom_smooth()
g + geom_hex(alpha=0.3) + geom_smooth(color="red", lwd=2) + geom_point(size=3) 

g <- ggplot(titanic, aes(Age))
g + geom_histogram()
g + geom_histogram(aes(fill=Sex))
g + geom_density()
g + geom_density(aes(fill=Sex))
g + geom_density(aes(fill=Sex), alpha=0.3)

ggplot(titanic, aes(Sex, Age)) + geom_boxplot()
ggplot(titanic, aes(as.factor(Pclass), fill=Sex))+geom_bar()
ggplot(titanic, aes(as.factor(Pclass), fill=as.factor(Survived)))+geom_bar()

titanic <- titanic %>% mutate(Pclass.factor = as.factor(Pclass), Survived.factor = as.factor(Survived), age.group = cut(Age, breaks=seq(0,90,10)) ) 
summary(titanic)

#(a) What plot would you draw for showing the distribution of all the values
#together? What conclusions would you draw?
#(b) Draw a graphic to show the number sailing in each class. What order of
#variable categories did you choose and why? Are you surprised by the different
#class sizes?
barchart <- ggplot(titanic, aes(as.factor(Pclass), fill=as.factor(Survived)))+geom_bar()
barchart+xlab("Passenger Class")+ylab("Number of Passengers")+ggtitle("Survival by Passenger Class and Gender")+scale_fill_discrete(name = "", labels = c("Died", "Survived"))

#(c) Draw graphics for the other three categorical variables. How good do you
#think these data are? Why are there not more detailed data on the ages of
#those sailing? Even if the age variable information (young and old) was
#accurate, is this variable likely to be very useful in any modelling?


#Exercise 6
#Swiss
#The dataset swiss contains a standardized fertility measure and various socioeconomic
#indicators for each of 47 French-speaking provinces of Switzerland in
#about 1888.
#(a) What plot would you draw for showing the distribution of all the values
#together? What conclusions would you draw?
swiss.df <- data.frame(swiss)
summary(swiss.df)
plot(swiss.df)



#(b) Draw graphics for each variable. What can you conclude from the distributions
#concerning their form and possible outliers?

h1 <- ggplot(swiss.df, aes(Fertility)) + geom_histogram()
h2 <- ggplot(swiss.df, aes(Agriculture)) + geom_histogram()
h3 <- ggplot(swiss.df, aes(Examination)) + geom_histogram()
h4 <- ggplot(swiss.df, aes(Education)) + geom_histogram()
h5 <- ggplot(swiss.df, aes(Catholic)) + geom_histogram()
h6 <- ggplot(swiss.df, aes(Infant.Mortality)) + geom_histogram()
grid.arrange(h1, h2, h3, h4, h5, h6, nrow=2)

#boxplot
library(dplyr)
SwissV <- select(swiss.df, Fertility:Infant.Mortality)
#par(mar=c(3.1, 4.1, 1.1, 2.1))
boxplot(scale(SwissV), pch=16, outcol="red")

#scatterplot with correlation values
library(GGally)
ggpairs(swiss.df, diag=list(continuous='density'),
        axisLabels='show')


#(c) Draw a scatterplot of Fertility against % Catholic. Which kind of
#areas have the lowest fertility rates?
with(swiss.df, {
  plot(Examination ~ Education)
  title("Examination vs. Education")
  plot(Catholic ~ Fertility)
  title("% Catholic vs. Fertility")
})

#(d) What sort of relationship is there between the variables Education and
#Agriculture?
ggplot(swiss.df, aes(x=Education, y=Agriculture)) + geom_point()
ggplot(swiss.df, aes(x=Education, y=Agriculture)) + geom_hex()
ggplot(swiss.df, aes(x=Education, y=Agriculture)) + geom_smooth()
ggplot(swiss.df, aes(x=Education, y=Agriculture)) + geom_line()
ggplot(swiss.df, aes(x=Education, y=Agriculture)) + geom_density2d()


#Painters
#The dataset painters in package MASS contains assessments of 54 classical
#painters on four characteristics: composition, drawing, colour, and expression.
#The scores are due to the eighteenth century art critic de Piles.
data(painters, package="MASS")
summary(painters)
plot(painters)
#(a) What plot would you draw for showing the distribution of all the values
#together? What conclusions would you draw?
#(b) Draw a display to compare the distributions of the four assessments. Is it
#necessary to scale the variables first? What information might you lose, if
#you did? What comments would you make on the distributions individually
#and as a set?
#(c) What would you expect the association between the scores for drawing and
#those for colour to be? Draw a scatterplot and discuss what the display
#shows in relation to your expectations
