
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(tidytuesdayR)
library(igraph)
library(ggraph)
library(widyr)


## (2) Loading the data and selecting countries ----
thanksgiving <- read.csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-20/thanksgiving_meals.csv")

tt_output <- tidytuesdayR::tt_load("2018-11-20")
thanksgiving <- tt_output$thanksgiving_meals


## (3) Creating gathered dataset and calculating correlations ----
food_gathered <- thanksgiving %>% 
  select(id, starts_with("side"),
         starts_with("pie"),
         starts_with("dessert")) %>% 
  select(-pie13, -side15, -dessert12) %>% 
  gather(type, value, -id) %>% 
  filter(!is.na(value),
         !value %in% c("None", "Other (please specify)")) %>% 
  mutate(type = str_remove(type, "\\d+")) # removing numbers, to use 

food_cors <- food_gathered %>% 
  widyr::pairwise_cor(value, id, sort = TRUE)

# calculate support vectors and numbers
n_respondents <- n_distinct(food_gathered$id)

food_types <- food_gathered %>% 
  count(value, type, sort = TRUE) # resorting is important

excluded_foods <- c("Fruit salad", "Vegetable salad", "Squash", "Cherry", "Coconut cream", "Buttermilk", "Brussel sprouts")

food_types2 <- food_types %>% filter(!value %in% excluded_foods)


## (4) Create final chart ----
final_chart <- food_cors %>% 
  head(75) %>% 
  filter(!item1 %in% excluded_foods,
         !item2 %in% excluded_foods) %>% 
  graph_from_data_frame(vertices = food_types2) %>% 
  ggraph() +
  geom_edge_link(alpha = .25, 
                 aes(width = correlation)) +
  geom_node_point(aes(color = type, size = n / n_respondents)) +
  geom_node_label(aes(label = name), 
                 vjust = 1, hjust = 1, repel = TRUE) + 
  scale_size_continuous(labels = scales::percent_format()) + 
  theme_void() +
  labs(title = "What foods get served together at Thanksgiving?",
       subtitle = "Based on a TidyTuesday screencast by @drob David Robinson",
       color = "Food type",
       size = "% of respondents", 
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest
       Data source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-11-20") + 
  guides(colour = guide_legend(override.aes = list(size = 6)))


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "17_connections_THXGIVING.png", 
       plot = final_chart, device = png,  
       width = 9, height = 9, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "17_connections_THXGIVING.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 9, height = 9, units = "in")
