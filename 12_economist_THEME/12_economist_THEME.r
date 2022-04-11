
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages and font  ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggthemes)

install.packages("remotes"); library(remotes)
remotes::install_github("hrbrmstr/hrbrthemes")
remotes::install_version("Rttf2pt1", version = "1.3.8")
install.packages("extrafont"); library(extrafont)
library(hrbrthemes)

font_import(path = "C:\\Users\\*user name*\\AppData\\Local\\Microsoft\\Windows\\Fonts", pattern = ".ttf")
loadfonts(device = "win")
fonts()
# "EconSansCndLig"


## (2) Creating the data ----
eco_data <- tibble::tribble(
  ~year, ~answer, ~percent, ~label_pos,
  2021,   "Yes",       20,    0,
  2021,   "Partially", 50,   20,
  2021,   "No",        30,   70,
  2022,   "Yes",       50,    0,
  2022,   "Partially", 30,   50,
  2022,   "No",        20,   80) %>% 
  mutate(year =  factor(year)) %>% 
  mutate(answer = factor(answer, levels = c("No", "Partially", "Yes")))


## (3) Creating the basic plot ----
basic_plot <- eco_data %>% ggplot(aes(x = percent, y = year, 
                                      fill = answer, label = answer)) +
  geom_col(position = "stack") +
  geom_text(aes(x = label_pos, y = year),
            color = "white", hjust = -0.1, size = 8, family = "EconSansCndReg")


## (4) Create final chart with annotations ----
blue <- "#1F5C99"; grey <- "#B3B09E"; red <- "#F6423C"
eco_colors <- c(red, grey, blue)

final_chart <- basic_plot +
  scale_fill_manual(values = eco_colors) + 
  scale_x_continuous(position = "top") + 
  theme_economist_white() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family="EconSansCndReg", size=14)) + 
  labs(x = "", y = "", title = "", subtitle = "Did you finish the #30DayChartChallenge?\nin % of respondents", 
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest")


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "12_economist_THEME.png", 
       plot = final_chart, device = png,  
       width = 4, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "12_economist_THEME.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 4, height = 8, units = "in")
