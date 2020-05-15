# ad_clicks contains the following columns:
#   
# user_id: unique user id
# utm_source: where user saw the ad. UTM stands for Urchin Tracking Module
# day: the day the ad was seen
# ad_click_timestamp: the time the ad was clicked
# ad_clicked: boolean indicating if ad was clicked (TRUE or FALSE)
# experimental_group: which ad version was shown (A or B)

# load packages
library(readr)
library(dplyr)
library(wakefield)

# load ad clicks data
setwd("~/Documents/Data Analytics/DataR/ShoeFly.com")
ad_clicks <- read_csv("ad_clicks.csv")
View(ad_clicks)

rows <- nrow(ad_clicks)

ad_clicked <- r_sample_logical(rows, prob = c(0.5,0.5), name = "Logical")
ad_clicked <- as.data.frame(ad_clicked)
View(ad_clicked)

ad_clicks <- cbind(ad_clicks, ad_clicked)
View(ad_clicks)

views_by_utm <- ad_clicks %>%
  group_by(utm_source) %>%
  summarize(count = n())
View(views_by_utm)

# define clicks_by_utm here:
clicks_by_utm <- ad_clicks %>%
  group_by(utm_source, ad_clicked) %>%
  summarize(count = n())
clicks_by_utm

# define percentage_by_utm here:
percentage_by_utm <- clicks_by_utm %>%
  group_by(utm_source) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(ad_clicked == TRUE)
percentage_by_utm

# define experiment_split here:
experiment_split <- ad_clicks %>%
  group_by(experimental_group) %>%
  summarize(count = n())
experiment_split

# define clicks_by_experiment here:
clicks_by_experiment <- ad_clicks %>%
  group_by(experimental_group, ad_clicked) %>%
  summarize(count = n())
clicks_by_experiment

# define a_clicks here:
a_clicks <- ad_clicks %>%
  group_by(experimental_group, ad_clicked) %>%
  #summarize(count = n()) %>%
  filter(experimental_group == "A")
a_clicks

# define b_clicks here:
b_clicks <- ad_clicks %>%
  group_by(experimental_group, ad_clicked) %>%
  #summarize(count = n()) %>%
  filter(experimental_group == "B")
b_clicks

# define a_clicks_by_day here
a_clicks_by_day <- a_clicks %>%
  group_by(day, ad_clicked) %>%
  summarize(count = n())
a_clicks_by_day

# define b_clicks_by_day here:
b_clicks_by_day <- b_clicks %>%
  group_by(day, ad_clicked) %>%
  summarize(count = n())
b_clicks_by_day

# define a_percentage_by_day here:
a_percentage_by_day <- a_clicks_by_day %>%
  mutate(percentage = count/sum(count)) %>%
  filter(ad_clicked == TRUE)
a_percentage_by_day


# define b_percentage_by_day here:
b_percentage_by_day <- b_clicks_by_day %>%
  mutate(percentage = count/sum(count)) %>%
  filter(ad_clicked == TRUE)
b_percentage_by_day