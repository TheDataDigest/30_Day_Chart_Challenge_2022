
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "janitor", "readxl", "readr", "scales", "purr")


## (2) Loading the data and creating a long data format ----
world_pop <- readr::read_csv(file = "UN-population-projection-medium-variant.csv") %>% 
  clean_names() %>% filter(entity == "World") %>% 
  mutate(population = ifelse(!is.na(population_historical_estimates), population_historical_estimates, population_future_projections)) %>% 
  select(year, population)


world_pop2 <- readr::read_csv(file = "world-population-by-world-regions-post-1820.csv") %>% 
  clean_names() %>% filter(entity == "World") %>% 
  select(year, population_historical_estimates) %>% 
  setNames(., nm = c("year", "population"))

world_pop <- rbind(filter(world_pop2, year %in% 1900:1949), world_pop)


## (3) Creating basic charts ----
poly <- world_pop

poly_past <- rbind(c(1900, 0), 
                   poly %>% select(year, population) %>% filter(year %in% 1900:2021),  
                   c(2021, 0))
  
poly_future <- rbind(c(2021, 0), 
                     poly %>% select(year, population) %>% filter(year %in% 2021:2100),  
                     c(2100, 0))

final_chart <- world_pop %>% ggplot(aes(x = year, y = population)) +
  geom_line(color = "brown", size = 2) + 
  geom_polygon(data = poly_past, aes(year, population), fill = "darkgreen", alpha = 0.8) +
  geom_polygon(data = poly_future, aes(year, population), fill = "lightgreen", alpha = 0.8) +
  theme_linedraw() + 
  scale_y_continuous(labels = function(x) paste(x/1e9, "billion")) + 
  labs(x = "Year", y = "World population", title = "Past and future world population size", caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://ourworldindata.org/world-population-growth")


## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "25_trends_WORLDPOP.png", 
       plot = final_chart, device = png,  
       width = 10, height = 5, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "25_trends_WORLDPOP.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 10, height = 5, units = "in", dpi = 200)


