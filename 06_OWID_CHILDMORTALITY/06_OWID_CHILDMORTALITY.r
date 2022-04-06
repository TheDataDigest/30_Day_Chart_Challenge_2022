

## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("devtools")  # Only needed if you don't have devtools installed
library(devtools)
devtools::install_github("drsimonj/ourworldindata")

library(easypackages)
easypackages::libraries("ourworldindata", "tidyverse", "lubridate", "countrycode", "maps")


#https://drsimonj.svbtle.com/ourworld-an-r-data-package
?ourworldindata

## (2) Loading and investigating the data ----
child_mortality %>% count(year, sort = FALSE)

child_mortality %>% 
  count(year) %>% 
  ggplot(aes(year, n)) +
  geom_col()

world <- map_data("world") %>% 
  filter(region != "Antarctica")


## (2b) Cleaning and filter data for mapping ----
table(child_mortality$continent, exclude = NULL)

# filter data and add count and intervals
owid_cm <- child_mortality %>% 
  filter(year >= 1800, year < 2000,
         !is.na(child_mort)) %>% 
  filter(!is.na(continent)) %>% # remove country == "world", "Upper middle income" etc
  add_count(country) %>% # add count to filter out low sample size
  filter(n == 200) %>% 
  mutate(interval = case_when(
    year >= 1950 ~ "1950-1999",
    year >= 1900 ~ "1900-1949",
    year >= 1850 ~ "1850-1899",
    year >= 1800 ~ "1800-1849"
  ))

# fixing country names for mapping
sort(unique(owid_cm$country[!owid_cm$country %in% world$region]))

owid_cm <- owid_cm %>% 
  filter(country != "Congo") %>% 
  mutate(country = case_when(
    country == "United States" ~ "USA",
    country == "United Kingdom" ~ "UK",
    country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Macedonia" ~ "North Macedonia",
    country == "Trinidad and Tobago" ~ "Trinidad",
    country == "Antigua and Barbuda" ~ "Antigua",
    TRUE ~ country)) %>% 
  filter(country %in% world$region)


## (3) Calculate average per intervall ----
interval_cm <- owid_cm %>% 
  group_by(interval, country) %>% 
  summarize(mean_cm = mean(child_mort, na.rm = T),
            median_cm = median(child_mort, na.rm = T))


## (4a) World map with continuous color scale ----
p_continuous <- interval_cm %>% 
  right_join(world, by = c(country = "region")) %>% 
  filter(country %in% owid_cm$country) %>% 
  ggplot(aes(long, lat, group = group, fill = mean_cm)) + 
  geom_polygon() +
  theme_linedraw() + 
  facet_wrap(~ interval) +
  coord_quickmap() +
  scale_fill_gradient2(low = "darkgreen", mid = "pink", 
                       high = "purple", midpoint = 300) +
  labs(title = "Child mortality rate: Country average over 50 year intervals",
       fill = "Deaths per\n100,000", x = "", y = "",
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: library(ourworldindata); child_mortality") + 
  theme(strip.text.x = element_text(size = 11), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())


## (4b) World map with binned color scale ----
# binned color scale
colors <-  colorRampPalette(c("darkgreen", "pink", "purple"))(7)

p_binned <- interval_cm %>% 
  right_join(world, by = c(country = "region")) %>% 
  filter(country %in% owid_cm$country) %>% 
  ggplot(aes(long, lat, group = group, fill = mean_cm)) + 
  geom_polygon() +
  theme_bw() + 
  facet_wrap(~ interval) +
  coord_quickmap() +
  scale_fill_stepsn(colours = colors, 
                    limits = round(c(min(interval_cm$mean_cm), max(interval_cm$mean_cm))), 
                    breaks = c(50, 100, 200, 300, 400, 500),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE)) +
  labs(title = "Child mortality rate: Country average over 50 year intervals",
       fill = "Deaths per\n100,000", x = "", y = "",
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: library(ourworldindata); child_mortality") + 
  theme(strip.text.x = element_text(size = 11), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "06_OWID_CHILDMORTALITY_continuous.png", 
       plot = p_continuous, device = png,  
       width = 12, height = 6, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "06_OWID_CHILDMORTALITY_binned.png", 
       plot = p_binned, device = png,  
       width = 12, height = 6, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "06_OWID_CHILDMORTALITY_continuous.pdf", 
       plot = p_continuous, device = cairo_pdf,  
       width = 12, height = 6, units = "in")

ggsave(path = getwd(), filename = "06_OWID_CHILDMORTALITY_binned.pdf", 
       plot = p_binned, device = cairo_pdf,  
       width = 12, height = 6, units = "in")
