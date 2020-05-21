# Load libraries
library(readr)
library(dplyr)
library(tidyverse)
library(DescTools) #Mode
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggpubr)
library(colorspace)


setwd("~/Documents/Data Analytics/DataR/Housing")

# Read in housing data
brooklyn_one_bed <- read_csv('brooklyn-one-bed.csv')
brooklyn_price <- brooklyn_one_bed$rent

manhattan_one_bed <- read_csv('manhattan-one-bed.csv')
manhattan_price <- manhattan_one_bed$rent

queens_one_bed <- read_csv('queens-one-bed.csv')
queens_price <- queens_one_bed$rent

#Calculate Mean
brooklyn_mean <- mean(brooklyn_price)
manhattan_mean <- mean(manhattan_price)
queens_mean <- mean(queens_price)

#Calculate Median
brooklyn_median <- median(brooklyn_price)
manhattan_median <- median(manhattan_price)
queens_median <- median(queens_price)

#Calculate Mode
brooklyn_mode <- Mode(brooklyn_price)
manhattan_mode <- Mode(manhattan_price)
queens_mode <- Mode(queens_price)

# Mean
if(exists('brooklyn_mean')) {
  print(paste("The mean price in Brooklyn is" , round(brooklyn_mean, digits=2))) 
}else{
  print("The mean price in Brooklyn is not yet defined.")
}

if(exists("manhattan_mean")) {
  print(paste("The mean price in Manhattan is", round(manhattan_mean,digits=2)))
} else {
  print("The mean in Manhattan is not yet defined.")
}
if(exists("queens_mean")) {
  print(paste("The mean price in Queens is" , round(queens_mean,digits=2)))
} else {
  print("The mean price in Queens is not yet defined.")
}   

# Median
if(exists("brooklyn_median")) {
  print(paste("The median price in Brooklyn is" , brooklyn_median)) 
}else{
  print("The median price in Brooklyn is not yet defined.")
}

if(exists("manhattan_median")) {
  print(paste("The median price in Manhattan is", manhattan_median))
} else {
  print("The median in Manhattan is not yet defined.")
}
if(exists("queens_median")) {
  print(paste("The median price in Queens is" , queens_median))
} else {
  print("The median price in Queens is not yet defined.")
} 

#Mode
if(exists("brooklyn_mode")) {
  print(paste("The mode price in Brooklyn is" , brooklyn_mode)) 
}else{
  print("The mode price in Brooklyn is not yet defined.")
}

if(exists("manhattan_median")) {
  print(paste("The mode price in Manhattan is", manhattan_mode))
} else {
  print("The mode in Manhattan is not yet defined.")
}
if(exists("queens_median")) {
  print(paste("The mode price in Queens is" , queens_mode))
} else {
  print("The mode price in Queens is not yet defined.")
} 

#It looks like the average cost of one-bedroom apartments
#in Manhattan is the most, and in Queens is the least. 
#This pattern holds for the median and mode values as well.

#While the mode is not the most important indicator of centrality, 
#the fact that mean, median, and mode are within a few hundred
#dollars for each borough indicates the data is centered around:
#$3,300 for Brooklyn
#$3,900 for Manhattan
#$2,300 for Queens

#We assumed that the data from Streeteasy is representative of housing 
#prices for the entire borough. Given that Streeteasy is only used by 
#a subset of property owners, this is not a fair assumption. A quick 
#search on rentcafe.com will tell you the averages are more like:
#$2,695 for Brooklyn one-bedroom apartments
#$4,188 for Manhattan one-bedroom apartments
#$2,178 for Queens one-bedroom apartments

#This is an interesting finding. Why may the cost from rentcafe.com be
#higher in Manhattan than in Brooklyn or Queens?
  
#Although we don’t have the answer to this question, it’s worth thinking
#about the possible differences between our Streeteasy data and where
#rentcafe is pulling their data.

Brooklyn <- cbind('Price' = brooklyn_one_bed$rent, City='Brooklyn')
Manhattan <- cbind('Price' = manhattan_one_bed$rent, City='Manhattan')
Queens <- cbind('Price' = queens_one_bed$rent, City = 'Queens')

data <- rbind(Brooklyn, Manhattan, Queens)
data <- as.data.frame(data)
data$City <- as.factor(data$City)
data$Price <- as.integer(data$Price)
#View(data)
attach(data)
type_of(Price)

p <- data %>%
  ggplot(aes(x=Price, fill=City)) +
  geom_histogram(binwidth=500, color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "red")) +
  labs(title="Prices of 1 Bedroom Apartment", subtitle="New York", y="Count", x="Prices") +
  xlim(0,10000) +
  theme_ipsum()
p

# plot
p1 <- data %>%
  #mutate(City = fct_reorder(City, Price)) %>%
  ggplot( aes(x=Price, color=City, fill=City)) +
  geom_histogram(alpha=0.6, binwidth = 500, color="white") +
  scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlim(0,10000) +
  labs(title="Prices of 1 Bedroom Apartment", subtitle="New York", y="Count", x="Prices") +
  facet_wrap(~City)
p1

pal <- choose_palette()
  
#plot data
vizb <- ggplot(data=brooklyn_one_bed, aes(x=brooklyn_one_bed$rent)) +
        geom_histogram(binwidth = 500, alpha = 0.5, color="white", fill = pal(21))+
        #scale_fill_manual(values=c(pal(1), pal(2), pal(3))) +
        labs(title="Prices of 1 Bedroom Apartment", subtitle="Brooklyn", 
        y="Count", x="prices") +
        geom_vline(aes(xintercept=median(brooklyn_one_bed$rent),
                 color="Median"), linetype="dashed",
             size=1) +
        geom_vline(aes(xintercept=mean(brooklyn_one_bed$rent),
                 color="Mean"), linetype="solid",
             size=1) +
        geom_vline(aes(xintercept=Mode(brooklyn_one_bed$rent),
                 color="Mode"), linetype = "dotdash",
             size=1)+
        scale_color_manual(name = "statistics", values = 
                             c(Median = "#10175B", Mean = "#B2A3FF", Mode = "#A1015D"))+
        xlim(0,10000)+
        theme_ipsum()

vizb

vizm <- ggplot(data=manhattan_one_bed, aes(x=manhattan_one_bed$rent)) +
  geom_histogram(binwidth = 500, alpha = 0.5, color="white", fill = pal(21))+
  #scale_fill_manual(values=c(pal(1), pal(2), pal(3))) +
  labs(title="Prices of 1 Bedroom Apartment", subtitle="Brooklyn", 
       y="Count", x="prices") +
  geom_vline(aes(xintercept=median(manhattan_one_bed$rent),
                 color="Median"), linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=mean(manhattan_one_bed$rent),
                 color="Mean"), linetype="solid",
             size=1) +
  geom_vline(aes(xintercept=Mode(manhattan_one_bed$rent),
                 color="Mode"), linetype = "dotdash",
             size=1)+
  scale_color_manual(name = "statistics", values = 
                       c(Median = Void[5], Mean = Void[4], Mode = "#A1015D"))+
  xlim(0,10000)+
  theme_ipsum()

vizm

vizq <- ggplot(data=queens_one_bed, aes(x=queens_one_bed$rent)) +
  geom_histogram(binwidth = 500, alpha = 0.5, color="white", fill = pal(21))+
  #scale_fill_manual(values=c(pal(1), pal(2), pal(3))) +
  labs(title="Prices of 1 Bedroom Apartment", subtitle="Brooklyn", 
       y="Count", x="prices") +
  geom_vline(aes(xintercept=median(queens_one_bed$rent),
                 color="Median"), linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=mean(queens_one_bed$rent),
                 color="Mean"), linetype="solid",
             size=1) +
  geom_vline(aes(xintercept=Mode(queens_one_bed$rent),
                 color="Mode"), linetype = "dotdash",
             size=1)+
  scale_color_manual(name = "statistics", values = 
                       c(Median = Void[5], Mean = Void[4], Mode = "#A1015D"))+
  xlim(0,10000)+
  theme_ipsum()

vizq

figure <- ggarrange(vizb, vizm, vizq,
                    ncol = 1, nrow = 3)

figure
