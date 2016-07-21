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
