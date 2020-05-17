# load libraries and data
library(readr)
library(dplyr)
library(tidyverse)
library(rlang)
library(ggpubr)
library(rstatix)
library(reshape2)
library(e1071)

# load ad clicks data
setwd("~/Documents/Data Analytics/DataR/FIFA")
FIFA19 <- read.csv("~/Documents/Data Analytics/DataR/FIFA/FIFA19.csv")
attach(FIFA19)
summary(FIFA19)
View(FIFA19)

n_distinct(Overall)

#Check for indentical values on BD
if (is_true(duplicated(FIFA19))){
  FIFA19 <- FIFA19 %>%
    distinct()
    message <- 'Deleted Duplicates'
} else{
  message <- 'All is well' 
}
print(message)

#First plot
viz1 <- ggplot(data=FIFA19)+
  geom_qq(aes(sample = Overall),alpha = 0.5, color=rainbow(1), fill=rainbow(1)) + 
  labs(title="Overall from Players", subtitle="From FIFA19 dataset", 
       y="Players", x="Overall")

geom_density(kernel = "gaussian")

#Second plot
viz2 <- ggplot(data=FIFA19, aes(x=Overall))+
  geom_density(kernel = "gaussian",alpha = 0.3, color=rainbow(1), fill=rainbow(1)) + 
  labs(title="Overall from Players", subtitle="From FIFA19 dataset", 
       y="Density", x="Overall")

viz2 + theme_minimal()

# plot hist
viz3 <- ggplot(data=FIFA19, aes(x=Overall)) +
  geom_histogram(binwidth = 1.0, alpha = 0.5, color="white", fill=rainbow(1+n_distinct(Overall))) + 
  labs(title="Overall from Players", subtitle="From FIFA19 dataset", 
       y="Players", x="Overall")

viz3 + theme_minimal()


#Testing normality on Overall

#Shapiro test: if the p-value > 0.05 implying that the distribution of the data 
#are not significantly different from normal distribution. In other words, we can 
#assume the normality.


set.seed(124)
x1<- sample(FIFA19$Overall, size=100, replace =F)
shapiro.test(x1)


#' @name assign_vector
#' @param data A vector of data to perform the t-test on.
#' @param n An integer indicating the number of t-tests to perform. Default is 1000
#' @return A data frame in "tall" format
assign_vector <- function(data, n = 1000) {
  # replicate the call to shapiro.test n times to build up a vector of p-values
  p.5 <- replicate(n=n, expr=shapiro.test(sample(data, 5, replace=TRUE))$p.value)
  p.10 <- replicate(n=n, expr=shapiro.test(sample(data, 10, replace=TRUE))$p.value)
  p.100 <- replicate(n=n, expr=shapiro.test(sample(data, 100, replace=TRUE))$p.value)
  p.1000 <- replicate(n=n, expr=shapiro.test(sample(data, 1000, replace=TRUE))$p.value)
  #' Combine the data into a data frame, 
  #' one column for each number of samples tested.
  p.df <- cbind(p.5, p.10, p.100, p.1000)
  p.df <- as.data.frame(p.df)
  colnames(p.df) <- c("S[5]", "S[10]", "S[100]", "S[1000]")
  #' Put the data in "tall" format, one column for number of samples
  #' and one column for the p-value.
  p.df.m <- melt(p.df, id.vars = NULL)
  #' Make sure the levels are sorted correctly.
  p.df.m <- transform(p.df.m, variable = factor(variable, levels = c("S[5]", "S[10]", "S[100]", "S[1000]")))
  return(p.df.m)  
}

n.test <- 10000
vector <- assign_vector(FIFA19$Overall, n = n.test)
View(vector)
#We would expect that normally distributed random data will have an equal 
#probability of any given p-value. i.e. 5% of the time we’ll see p-value 
#≤ 0.05, 5% of the time we’ll see p-value > 0.05 and ≤ 0.10, and so on 
#through > 0.95 and ≤ 1.00. Let’s graph that and see what we get for each sample size:

viz4 <- ggplot(vector, aes(x = value)) + 
  geom_histogram(binwidth = 1/50, alpha = 0.5, color="white", fill=rainbow(204)) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  #xlim(0,1) +
  labs(title="Running Normality Tests with Shapiro-Wilk", subtitle="Variable: Overall", 
       y="Count of p-values", x="p-values") +
  theme_minimal() +
  theme(axis.title=element_text(size=14,face="bold"),
        text = element_text(size = 16))

viz4

#Checking the tails
summary(Overall)
skewness(Overall)
kurtosis(Overall) 

#the value of kurtosis is negative so overall is platykurtic
#probably because some outliers are present


