library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(plotrix)
library(ggpubr)
library(rlang)
library(ggpubr)
library(rstatix)
library(reshape2)
library(e1071)
library(viridis)
library(hrbrthemes)

setwd("~/Documents/Data Analytics/DataR/Museum")
museums_df <- read.csv("museums.csv")
View(museums_df)
attach(museums_df)


museum_type <- ggplot(museums_df, aes(x = Museum.Type)) +
  geom_bar(fill = rainbow(9)) + 
  labs(title="Type of Museums", subtitle="Variable: Museum.Type") + 
  theme_minimal() + 
  theme(text = element_text(size = 9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(labels = scales::wrap_format(8))
 
museum_type

museum_class <- ggplot(museums_df, aes(x = factor(Is.Museum))) +
  geom_bar(fill = c("#CC6666", "#9999CC")) +
  labs(title="Museums", subtitle="Variable: Is.Museum") +
  scale_x_discrete(labels = c("TRUE" = "Museum", "FALSE" = "Non-museum")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
museum_class

museums_states <-  museums_df %>% 
  filter(State..Administrative.Location. %in% c("IL", "CA", "NY"))
View(museums_states)

museum_facet <- ggplot(museums_states, aes(x = factor(Is.Museum))) +
  facet_grid(cols = vars(State..Administrative.Location.)) +
  geom_bar(fill = c('#957DAD','#957DAD','#B09EB2','#B09EB2',"#FEC8D8", "#FEC8D8")) +
  labs(title="Museums", subtitle="Variable: Is.Museum") +
  scale_x_discrete(labels = c("TRUE" = "Museum", "FALSE" = "Non-museum")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

museum_facet

museum_stacked <- ggplot(museums_states, aes(x = factor(Region.Code..AAM.), fill = factor(Is.Museum))) +
  geom_bar(position = "fill", colour = "grey") +
  labs(title="Museum Types by Region", subtitle="Variable: Region.Code..AAM.",x = "Region", y = "Percentage of Total", fill = "Type") +
  scale_x_discrete(labels = c("TRUE" = "Museum", "FALSE" = "Non-museum")) +
  theme_ipsum() +
  scale_x_discrete(labels = c("1" = "New England",
                              "2" = "Mid-Atlantic",
                              "3" = "Southeastern",
                              "4" = "Midwest",
                              "5" = "Mountain Plains",
                              "6" = "Western"))+
  scale_fill_discrete(labels = c("TRUE" = "Museum", "FALSE" = "Non-museum")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis(discrete = T)

museum_stacked

#                  Explore Institutions by Revenue

museums_revenue_df <- museums_df %>%
  distinct(Legal.Name, .keep_all = TRUE) %>%
  filter(Annual.Revenue > 0)
View(museums_revenue_df)

museums_small_df <- museums_revenue_df %>%
  filter(Annual.Revenue < 1000000)
View(museums_small_df)

museums_large_df <- museums_revenue_df %>%
  filter(Annual.Revenue > 1000000000)

revenue_histogram <- ggplot(museums_small_df, aes(x = Annual.Revenue)) +
  geom_histogram(binwidth = 100000, alpha = 0.5, color="white", fill=rainbow(11)) + 
  labs(title="Museums with Small Anual Revenue", subtitle="Variable: Annual.Revenue") + 
  theme_minimal() +
  theme(text = element_text(size = 9)) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_continuous(labels = scales::dollar_format())
  #scale_x_discrete(labels = scales::wrap_format(8))
revenue_histogram  

revenue_boxplot <- ggplot(museums_large_df, aes(x=factor(Region.Code..AAM.), y=Annual.Revenue)) + 
  geom_boxplot(fill=rainbow(6), alpha=0.2) +
  scale_x_discrete(labels = c("1" = "New England",
                              "2" = "Mid-Atlantic",
                              "3" = "Southeastern",
                              "4" = "Midwest",
                              "5" = "Mountain Plains",
                              "6" = "Western"))+
  coord_cartesian(ylim = c(1e9, 3e10)) +
  theme_minimal() +
  labs(title="Museums with Big Anual Revenue", subtitle="Variable: Annual.Revenue") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_y_continuous(labels = function(x) paste0("$", x/1e9, "B"))

revenue_boxplot

revenue_barplot <- ggplot(museums_revenue_df, aes(x=factor(Region.Code..AAM.), y=Annual.Revenue)) + 
  geom_bar(stat = "summary", fun = "mean",fill=rainbow(6), alpha=0.5) +
  scale_x_discrete(labels = c("1" = "New England",
                              "2" = "Mid-Atlantic",
                              "3" = "Southeastern",
                              "4" = "Midwest",
                              "5" = "Mountain Plains",
                              "6" = "Western")) +
  theme_minimal() +
  labs(title="Mean Annual Revenue by Region", subtitle="Variable: Region.Code..AAM.",
       x = "Region", y = "Mean Annual Revenue") + 
  scale_y_continuous(labels = function(x) paste0("$", x/1e7, "B"))
revenue_barplot

museums_error_df <- museums_revenue_df %>%
  group_by(Region.Code..AAM.) %>%
  summarize(
    Mean.Revenue = mean(Annual.Revenue), 
    Mean.SE = std.error(Annual.Revenue)) %>%
  mutate(
    SE.Min = Mean.Revenue - Mean.SE, 
    SE.Max = Mean.Revenue + Mean.SE)

revenue_errorbar <- ggplot(museums_error_df, aes(x=factor(Region.Code..AAM.), y = Mean.Revenue)) + 
  geom_bar(stat = "identity",fill=rainbow(6), alpha=0.5) + 
  geom_errorbar(aes(ymin = SE.Min, ymax = SE.Max), width = 0.2) +
  scale_x_discrete(labels = c("1" = "New England",
                              "2" = "Mid-Atlantic",
                              "3" = "Southeastern",
                              "4" = "Midwest",
                              "5" = "Mountain Plains",
                              "6" = "Western")) +
  theme_minimal() +
  labs(title="Mean Annual Revenue by Region", subtitle="Variable: Region.Code..AAM.",
       x = "Region", y = "Mean Annual Revenue") + 
  scale_y_continuous(labels = function(x) paste0("$", x/1e7, "B"))

revenue_errorbar
