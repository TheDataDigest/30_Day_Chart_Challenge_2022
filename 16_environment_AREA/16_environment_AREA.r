
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggcharts)
library(readxl)
library(janitor)
library(ggthemes)
library(scales)


## (2) Loading the data and selecting countries ----
area <- read_excel(path = paste0(getwd(), "/PROTECTED_AREAS_OECD_Stat.xlsx"), sheet = 1) %>% clean_names()
countries <- c("Spain", "Italy", "Germany", "Austria", "Brazil", "World", "Sweden", "South Africa", "Costa Rica", "Argentina", "Mexico")


## (3) Creating the basic plot ----
final_chart <- area %>% filter(country %in% countries) %>% 
  dumbbell_chart(
  x = country,
  y1 = x2001,
  y2 = x2021,
  point_colors = c("lightgray", "#494F5C"),
  legend_labels = c("2001", "2021")
) +
  labs(x = NULL, y = NULL,
       title = "Change in protected land area from 2001 to 2021",
       subtitle = "Terrestrial protected areas (% of total land area)"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) paste(x, "%")
  )


## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "16_environment_AREA.png", 
       plot = final_chart, device = png,  
       width = 7, height = 7, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "16_environment_AREA.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 7, height = 7, units = "in")
