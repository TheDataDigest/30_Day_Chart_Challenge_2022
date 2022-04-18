
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("ourworldindata", "tidyverse", "lubridate", "countrycode", "maps", "readxl", "gapminder", "patchwork", "ggcharts")


## (2) Loading the data and selecting countries ----
OECD_members <- readxl::read_excel(path = paste0(getwd(), "/OECD_data.xlsx"), sheet = "membership")

# check spelling of country names for OECD members availability in financing_healthcare
OECD_members$country[!OECD_members$country %in% financing_healthcare$country]

# check spelling of country names for OECD members availability in world maps
world <- map_data("world") %>% 
  filter(region != "Antarctica")

OECD_members$country[!OECD_members$country %in% world$region]

# prepare data for cleveland plots 
data_fh <- financing_healthcare %>% 
  mutate(country = case_when(
    country == "United States" ~ "USA",
    country == "United Kingdom" ~ "UK",
    TRUE ~ country)) %>% 
  filter(country %in% OECD_members$country) %>% 
  filter(year %in% c(1970, 2013)) %>% 
  select(country, year, child_mort, life_expectancy) %>% 
  pivot_longer(cols = -c(country, year)) %>% 
  pivot_wider(id_cols = c(country, name), names_from = year, values_from = value)

world <- world %>% 
  mutate(region = case_when(
    region == "United States" ~ "USA",
    region == "United Kingdom" ~ "UK",
    TRUE ~ region))


## (3) Creating basic plots ----

# map of membership years
colors <-  colorRampPalette(c("darkgreen", "pink", "purple"))(5)

map <- OECD_members %>% 
  right_join(world, by = c(country = "region")) %>% 
  ggplot(aes(long, lat, group = group, fill = year, label = country)) + 
  geom_polygon(color = "grey", size = 0.2) +
  theme_bw() + 
  scale_fill_stepsn(colours = colors, 
                    limits = c(1961, 2021), 
                    breaks = c(1961, 1964, 1973, 1996, 2010, 2021),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE)) +
  labs(title = "Year of joining OECD (Organisation for Economic Co-operation and Development)",
       subtitle = "There were 20 founding members in 1961. 18 more countries joined later.",
       fill = "Year", x = "", y = "") + 
  theme(strip.text.x = element_text(size = 11), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) #+
  #coord_fixed()

# cleveland plot 1 (child mortality)
p_child <- data_fh %>% filter(name == "child_mort") %>% 
  dumbbell_chart(
    x = country,
    y1 = `1970`,
    y2 = `2013`,
    line_size = 1, point_size = 2,
    point_colors = c("lightgray", "#494F5C"),
    legend_labels = c("1970", "2013"),
    sort = T) +
  labs(x = NULL, y = NULL,
       title = "Child mortality from 1970 to 2013",
       subtitle = "Cases per 100,000") +
  scale_y_continuous(limits = c(0, NA),
    labels = function(x) paste(x, "per 100k")) + 
  theme(axis.text.x = element_text(size = 9), 
        axis.text.y = element_text(size = 10),
        axis.ticks = element_blank()) 

# cleveland plot 2(life expectancy)
p_life <- data_fh %>% filter(name == "life_expectancy") %>% 
  dumbbell_chart(
    x = country,
    y1 = `1970`,
    y2 = `2013`,
    line_size = 1, point_size = 2,
    point_colors = c("lightgray", "#494F5C"),
    legend_labels = c("1970", "2013"),
    sort = T) +
  labs(x = NULL, y = NULL,
       title = "Life expectancy from 1970 to 2013",
       subtitle = "In years") +
  scale_y_continuous(labels = function(x) paste(x, "years")) + 
  theme(axis.text = element_text(size = 10), 
        axis.ticks = element_blank()) 

  
## (4) Combine plots with patchwork ----
design <- "
11
11
23
23
"

final_chart <-  map + p_child + p_life + 
  patchwork::plot_layout(design = design) + 
  patchwork::plot_annotation(
    caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: library(ourworldindata) | https://en.wikipedia.org/wiki/OECD")
  

## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "18_OECD.png", 
       plot = final_chart, device = png,  
       height = 12, width = 10, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "18_OECD.pdf", 
       plot = final_chart, device = cairo_pdf,  
       height = 12, width = 10, units = "in")
