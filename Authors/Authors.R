# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(DescTools) #Mode

# load data frame
setwd("~/Documents/Data Analytics/DataR/Authors")
greatest_books <- read.csv("~/Documents/Data Analytics/DataR/Authors/Authors.csv")

#plot data
hist <- qplot(greatest_books$Ages,
              geom='histogram',
              binwidth = 3,  
              main = 'Age of Top 100 Novel Authors at Publication', 
              xlab = 'Publication Age',
              ylab = 'Count',
              fill=I("blue"), 
              col=I("white"), 
              alpha=I(.2)) +
  geom_vline(aes(xintercept=median(greatest_books$Ages),
                 color="median"), linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=mean(greatest_books$Ages),
                 color="mean"), linetype="solid",
             size=1) +
  scale_color_manual(name = "statistics", values = c(median = "purple", mean = "pink"))

hist

# Set author ages to a vector
author_ages <- greatest_books$Ages

# Use R to calculate mean
average_age <- mean(author_ages)
average_age

#There are always two steps to finding the median of a dataset:
#1. Order the values in the dataset from smallest to largest
#2. Identify the number(s) that fall(s) in the middle

# Save author ages to author_ages
author_ages <- greatest_books$Ages

# Use R to calculate the median age of the top 100 authors
median_age <- median(author_ages)

print(paste("The median age of the 100 greatest authors, according to a survey by Le Monde is: " , median_age))

mode_age <- Mode(author_ages)

print(paste("The mode age of authors from Le Monde's 100 greatest books is: ", mode_age[1]))
