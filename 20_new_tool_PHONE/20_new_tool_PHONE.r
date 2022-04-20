
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


# recreate this chart:
# https://www.pewresearch.org/global/2019/02/05/smartphone-ownership-is-growing-rapidly-around-the-world-but-not-always-equally/

## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(tidytuesdayR)
library(countrycode)


## (2) Loading the data and creating a subset for highlights ----
raw_mobile <- read_csv("2020/2020-11-10/mobile-phone-subscriptions-vs-gdp-per-capita.csv")

tt_output <- tidytuesdayR::tt_load("2020-11-10")
mobile <- tt_output$mobile
mobile$country <- countrycode(sourcevar = mobile$code, origin = 'iso3c', destination = 'country.name')

subset_df <- mobile %>% 
  filter(country %in% c("Tunisia", "Angola", "Canada", "Costa Rica", "Japan", "Afghanistan", "Austria", "Romania"),
         year %in% 1995:2015)


## (3) Creating final chart ----
final_chart <- mobile %>% filter(continent != "Oceania",
                  !is.na(country),
                  year %in% 1995:2015) %>% 
  ggplot(aes(x = year, y = mobile_subs, group = country)) +
  geom_line(color = "grey") +
  geom_line(data = subset_df, aes(x = year, y = mobile_subs, color = country)) + 
  geom_text(data = subset_df %>% filter(year == 2015), 
            aes(x = year, y = mobile_subs, label = country), 
            size = 2.8, nudge_x = 0.1, hjust = -0.1) + 
  facet_wrap(~ continent) +
  theme_bw() +
  scale_x_continuous(limits = c(1995, 2019), labels = seq(from = 1995, to = 2015, by = 5),
                     breaks = seq(from = 1995, to = 2015, by = 5)) + 
  theme(legend.position = "none") +
  labs(x = "Year", y = "Fixed mobile subscriptions (per 100 people)", title = "Fixed mobile subscriptions (per 100 people)",
       subtitle = "With two countries highlighted per continent (1995 to 2015)", 
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://ourworldindata.org/technology-adoption")


## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "20_new_tool_PHONE.png", 
       plot = final_chart, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "20_new_tool_PHONE.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 8, units = "in")
