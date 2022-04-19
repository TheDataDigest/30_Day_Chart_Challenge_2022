
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(gapminder)


## (2) Loading the data and creating a subset for highlights ----
data(gapminder)

subset_df <- gapminder %>% 
  filter(country %in% c("Tunisia", "Angola", "Canada", "Bolivia", "Japan", "Afghanistan", "Spain", "Romania"))


## (3) Creating final chart ----
final_chart <- gapminder %>% filter(continent != "Oceania") %>% 
  ggplot(aes(x = year, y = lifeExp, group = country)) +
  geom_line(color = "grey") +
  geom_line(data = subset_df, aes(x = year, y = lifeExp, color = country)) + 
  geom_text(data = subset_df %>% filter(year == 2007), 
            aes(x = year, y = lifeExp, label = country), 
            size = 2.8, nudge_x = 0.1, hjust = -0.1) + 
  facet_wrap(~ continent) +
  theme_bw() +
  scale_x_continuous(limits = c(1950, 2017), labels = c(1952, 1962, 1972, 1982, 1992, 2007),
                     breaks = c(1952, 1962, 1972, 1982, 1992, 2007)) + 
  theme(legend.position = "none") +
  labs(x = "Year", "Life expectancy in years", title = "Global change of life expectancy",
       subtitle = "With two countries highlighted per continent (1952 to 2007)", 
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: library(gapminder)")


## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "19_global_change.png", 
       plot = final_chart, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "19_global_change.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 8, units = "in")
