---
  title: "Page Visits Funnel"
output: html_notebook
---
  
# load packages
library(readr)
library(dplyr)

# load data
visits <- read_csv("visits.csv")
cart <- read_csv("cart.csv")
checkout <- read_csv("checkout.csv")
purchase <- read_csv("purchase.csv")

# inspect data frames
head(visits)
head(cart)
head(checkout)
head(purchase)

# define visits_cart here:
visits_cart <- visits %>%
  left_join(cart)
visits_cart

# define total_visits here:
total_visits <- nrow(visits)

# define visit_no_cart here:
visit_no_cart <- visits_cart %>%
  filter(is.na(cart_time))

# calculate visit_no_cart_percent here:
visit_no_cart_percent <- (nrow(visit_no_cart)*100)/total_visits
visit_no_cart_percent

# define cart_checkout here:
cart_checkout <- cart %>%
  left_join(checkout)
cart_checkout

# define total_carts here:
total_carts <- nrow(cart_checkout)

# define cart_no_checkout here:
cart_no_checkout <- cart_checkout %>%
  filter(is.na(checkout_time))

# calculate cart_no_checkout_percent here:
cart_no_checkout_percent <- nrow(cart_no_checkout)*100/total_carts
cart_no_checkout_percent

# define all_data here:
all_data <- visits %>%
  left_join(cart) %>%
  left_join(checkout) %>%
  left_join(purchase)
head(all_data)

# define total_checkout here:
total_checkout <- nrow(all_data)

# define checkout_no_purchase here:
checkout_no_purchase <- all_data %>%
  filter(is.na(checkout_time), is.na(purchase_time))

# calculate checkout_no_purchase_percent here:
checkout_no_purchase_percent <- nrow(checkout_no_purchase)*100/total_checkout
checkout_no_purchase_percent

#Which step of the funnel is weakest
#(i.e., has the highest percentage of
#users not completing it)?

#How might Cool T-Shirts Inc. change
#their website to fix this problem

# update all_data with time_to_purchase column here:
all_data <- all_data %>%
  mutate(time_to_purchase = purchase_time - visit_time)

# inspect the updated all_data data frame here:
head(all_data)

# define time_to_purchase here:
time_to_purchase <- all_data %>%
  summarize(mean_time = mean(time_to_purchase, na.rm = TRUE))
time_to_purchase
#the average time from initial visit to final purchase. 