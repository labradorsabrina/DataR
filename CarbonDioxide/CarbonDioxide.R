#From: Codeacademy course R
# Climate scientists have measured the concentration of carbon dioxide (CO2) 
# in the Earth’s atmosphere dating back thousands of years. In this project, 
# we will investigate two datasets containing information about the carbon 
# dioxide levels in the atmosphere. We will specifically look at the increase 
# in carbon dioxide levels over the past hundred years relative to the variability 
# in levels of CO2 in the atmosphere over eight millennia.
# 
# These data are calculated by analyzing ice cores. Over time, gas gets trapped 
# in the ice of Antarctica. Scientists can take samples of that ice and see how 
# much carbon is in it. The deeper you go into the ice, the farther back in time 
# you can analyze!
#   
# The first dataset comes from World Data Center for Paleoclimatology, Boulder
# and NOAA Paleoclimatology Program and describes the carbon dioxide levels back
# thousands of years “Before Present” (BP) or before January 1, 1950.
# 
# The second dataset explores carbon dioxide starting at year zero up until the 
# recent year of 2014. This dataset was compiled by the Institute for Atmospheric
# and Climate Science (IAC) at Eidgenössische Technische Hochschule in Zürich, Switzerland.
# 
# In order to understand the information in these datasets, it’s important to understand 
# two key facts about the data:
#   
#   The metric for carbon dioxide level is measured as parts per million or CO2 ppmv. 
#   This number describes the number of carbon dioxide molecules per one million gas 
#   molecules in our atmosphere.
#   
#   The second metric describes years before present, which is “a time scale used 
#   mainly in … scientific disciplines to specify when events occurred in the past… 
#   standard practice is to use 1 January 1950 as the commencement date of the age scale, 
#   reflecting the origin of practical radiocarbon dating in the 1950s. The abbreviation 
#   “BP” has alternatively been interpreted as “Before Physics” that is, before nuclear 
#   weapons testing artificially altered the proportion of the carbon isotopes in the 
#   atmosphere, making dating after that time likely to be unreliable.” This means that
#   saying “the year 20 BP” would be the equivalent of saying “The year 1930.”

library(readr)
library(dplyr)
library(ggplot2)

options(scipen=10000)

setwd("~/Documents/Data Analytics/DataR/CarbonDioxide")
noaa_data <- read.csv("carbon_dioxide_levels.csv")
View(noaa_data)

str(noaa_data)

noaa_viz <- ggplot(data=noaa_data, aes(x=Age_yrBP, y=CO2_ppmv)) +
  #geom_line(color=topo.colors(1096)) +
  geom_line(color=rainbow(1096)) +
  labs(title = "Carbon Dioxide Levels From 8,000 to 136 Years BP", subtitle = "From World Data Center for Paleoclimatology and NOAA Paleoclimatology Program", x="Years Before Today (0=1950)", y= "Carbon Dioxide Level (Parts Per Million)")

noaa_viz + scale_x_reverse(lim=c(800000,0))

iac_data <- read.csv("yearly_co2.csv")
str(iac_data)
head(iac_data)

iac_viz <- ggplot(data=iac_data, aes(x=year, y=data_mean_global)) +
  geom_line(color=rainbow(2015)) +
  labs(title = "Carbon Dioxide Levels From 8,000 to 136 Years BP", subtitle = "From Institute for Atmospheric and Climate Science (IAC)", x="Years", y= "Carbon Dioxide Level (Parts Per Million)")

millennia_max <- max(noaa_data$CO2_ppmv)
millennia_max

iac_viz + geom_hline(aes(yintercept=millennia_max, linetype= "Historical CO2 Peak before 1950"))
