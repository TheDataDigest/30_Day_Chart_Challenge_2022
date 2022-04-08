

## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

Sys.setenv(LANG = "en")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "readxl", "lubridate", "janitor", "stringr", "forcats", "RColorBrewer")


## (2) Loading and cleaning the data ----
# Source: https://www.himalayandatabase.com/scripts/peaksmtr.php

bu <- readxl::read_xlsx(path = paste0(getwd(), "/Everest.xlsx"))

everest <- readxl::read_xlsx(path = paste0(getwd(), "/Everest.xlsx")) %>% 
  mutate(year = parse_number(`Yr/Seas`),
         date = parse_date_time(Date, orders = "Omd"), # format for "May 29" etc.
         month = month(date),
         day = day(date),
         date =  make_date(year, month, day),
         sea = stringr::str_extract(`Yr/Seas`, pattern = "[A-z]+"),
         season = case_when(
           sea == "Aut" ~ "autumn",
           sea == "Spr" ~ "spring",
           sea == "Sum" ~ "summer",
           sea == "Win" ~ "winter"),
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"), ordered = TRUE)) %>% 
  select(-Date1) %>% 
  janitor::clean_names()


## (3) Manipulating the data, preparation for the plot ----
everest %>% ggplot(aes(x = year)) + geom_bar()

lifts_df <- tables %>% 
  filter(Bodyweight != "All") %>% 
  pivot_longer(-c(Bodyweight, lift)) %>% 
  mutate(bodyweight_kg = parse_number(Bodyweight),
         barbell_kg = parse_number(value),
         barbell_lb = barbell_kg * 2.20462, 
         bodyweight_lb = bodyweight_kg * 2.20462,
         level = factor(name, 
                        levels = c("Beginner", "Novice", "Intermediate", "Advanced", "Elite"),
                        ordered = TRUE)) %>%  
  select(lift, level, bodyweight_kg, barbell_kg, bodyweight_lb, barbell_lb)


## (4) Basic plots ----
# yearly summit ascents
ascents <- everest %>% 
  ggplot(aes(x = year)) + geom_bar() +
  labs(x = "Year", y = "Number of ascents per year") + 
  theme_bw()

# age and sex distribution
age_sex <- everest %>% 
  filter(age > 0) %>% 
  ggplot(aes(x = age, fill = sex)) + #geom_bar(position = "dodge") +
  geom_bar() +
  labs(x = "Year", y = "Number of people that made it to the top") + 
  theme_bw() +
  facet_wrap(~sex, ncol = 1) +
  theme(legend.position = "none")
  
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
  #scale_y_log10() +
  #scale_fill_manual(values = brewer.pal(11, "Set3")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 11))


## (5) Advanced plot with patchwork ----

design <- "
  11
  23
  23
"

final_chart <-  ascents + age_sex + citizenship +
  patchwork::plot_layout(design = design) + 
  patchwork::plot_annotation(
    title = "title",
    subtitle = "subtitle", 
    caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://www.himalayandatabase.com/")


## (6) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "08_mountain_EVEREST.png", 
       plot = final_chart_kg, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "08_mountain_EVEREST.pdf", 
       plot = final_chart_kg, device = cairo_pdf,  
       width = 8, height = 8, units = "in")


