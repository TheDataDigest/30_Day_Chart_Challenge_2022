

## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "readxl", "lubridate", "janitor", "stringr", "forcats", "RColorBrewer")


## (2) Loading and cleaning the data ----
# Source: https://www.himalayandatabase.com/scripts/peaksmtr.php

everest <- readxl::read_xlsx(path = paste0(getwd(), "/Everest.xlsx")) %>% 
  mutate(year = parse_number(`Yr/Seas`),
         date = parse_date_time(Date, orders = "Omd"), # format for "May 29" etc.
         month = month(date),
         day = day(date),
         date =  make_date(year, month, day),
         Sex = case_when(
           Sex == "M" ~ "Males",
           Sex == "F" ~ "Females"),
         Sex = factor(Sex, levels = c("Males", "Females")),
         sea = stringr::str_extract(`Yr/Seas`, pattern = "[A-z]+"),
         season = case_when(
           sea == "Aut" ~ "autumn",
           sea == "Spr" ~ "spring",
           sea == "Sum" ~ "summer",
           sea == "Win" ~ "winter"),
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"), ordered = TRUE)) %>% 
  select(-Date1) %>% 
  janitor::clean_names()


## (3) Basic plots ----
# yearly summit ascents
ascents <- everest %>% 
  ggplot(aes(x = year)) + geom_bar(fill = "cyan", color = "grey") +
  labs(title = "Number of ascents per year", x = "Year", y = "") + 
  theme_bw() +
  theme(axis.text = element_text(size = 11))

# age and sex distribution
age_sex <- everest %>% 
  filter(age > 0) %>% 
  ggplot(aes(x = age, fill = sex)) + #geom_bar(position = "dodge") +
  geom_bar() +
  theme_bw() +
  facet_wrap(~sex, ncol = 1, scales = "free_y") +
  theme(legend.position = "none",
        axis.text = element_text(size = 11)) +
  labs(title = "Age and sex of climbers", 
       x = "Age in years", y = "Number of climbers")
  
# citizenship
citizenship <- everest %>% 
  mutate(citizenship = factor(citizenship),
         citizenship = fct_lump(citizenship, n = 15)) %>% 
  add_count(citizenship) %>% 
  mutate(citizenship = fct_reorder(citizenship, n)) %>% 
  count(citizenship) %>% 
  ggplot(aes(x = citizenship, y = n, fill = citizenship)) + 
  geom_col() + 
  ylim(c(0, 6000)) + 
  geom_label(aes(y = n, x = citizenship, label = n), hjust = - 0.2, size = 3) +
  coord_flip() +
  theme_bw() +
  labs(title = "Citizenship of climbers", 
       x = "", y = "Number of climbers") + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11))


## (4) Advanced plot with patchwork ----

design <- "
  11
  23
  23
"

final_chart <-  ascents + age_sex + citizenship +
  patchwork::plot_layout(design = design) + 
  patchwork::plot_annotation(
    title = "There have been 10656 ascents to Mount Everest (8848m) since 1953",
    subtitle = "The charts below show the distribution of ascents per year, the age/sex and citizenship of the climbers", 
    caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://www.himalayandatabase.com/")


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "08_mountain_EVEREST.png", 
       plot = final_chart, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "08_mountain_EVEREST.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 8, units = "in")


