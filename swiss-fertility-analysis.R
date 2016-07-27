################
#
# Another data example: the Swiss fertility data
#   47 French speaking provinces of Switzerland around 1888
#
#           Fertility     - standardized fertility measure for each province 
#           Agriculture   - population involved in agriculture 
#           Examiniation  - draftees receiving highest mark in army exam 
#           Education     - population educated beyond primary school 
#           Catholic      - population who are catholic 
#           Infant.Mort   - live births living less than 1 year 

# The interest is to relate Fertility (the response) to the other variables
# (the predictors).  
# The hypothesis is that the predictors are good proxies for the true causes
# of high and low fertility.
data(swiss)
swiss
names(swiss)
dim(swiss)

# For my own interest, I wanted to know the names of some of these provinces.
# especially those that are high in Education and Fertility.
# To this end, we obtain an index vector "ord" with the "order" function
# to sort the data, first according to Education, then Fertility:
#High Education
ord <- rev(order(swiss[,"Education"]))
rownames(swiss)[ord][1:7]
#High Fertility
ord <- rev(order(swiss[,"Fertility"]))
rownames(swiss)[ord][1:10]
#Low Fertility
ord <- order(swiss[,"Fertility"])
rownames(swiss)[ord][1:10]

#Basic plots of Fertility vs. predictors
windows(width=8, height=12)
par(pch=15, mfrow=c(3,2), mar=rep(2,4), cex=1.2)
plot(swiss$Agriculture,swiss$Fertility, main="Agriculture")
plot(swiss$Examination,swiss$Fertility, main="Examination")
plot(swiss$Education,swiss$Fertility, main="Education")
plot(swiss$Catholic,swiss$Fertility, main="Catholic")
plot(swiss$Infant.Mort,swiss$Fertility, main="Infant.Mort")

library("psych")
pairs.panels(swiss)

plot(swiss[,c("Catholic","Fertility")], pch=16)
for(i in 1:3)
  identify(swiss[,c("Catholic","Fertility")], labels=rownames(swiss))
# Click the three bottom points in the plot...
#
# These marginal findings call for an investigation of "collinearities",
# that is, dependencies among the predictors to answer questions of whether
# Examination and Education are indeed proxy for each other, for example.
# The easiest approach is with a scatterplot matrix:
pairs(swiss, pch=16)

# Right away we see that Examination and Education are indeed
# positively correlated, that Agriculture negatively correlates
# with both, although more so with Examination, and that
# Infant.Mort does not correlate with anything very much.
library(tree)
swiss.Scart <- tree(Fertility ~ ., data=swiss,
                    control=tree.control(47, mincut=3, minsize=6))
plot(swiss.Scart, type="u");text(swiss.Scart)
