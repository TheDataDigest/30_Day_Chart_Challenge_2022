
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData

# https://worldpopulationreview.com/world-cities

## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "janitor", "readxl", "readr", "scales", "purr", "glue")

## (2) Loading the data ----
cities <- read_excel(path = paste0(getwd(), "/world_cities.xlsx")) %>% 
  mutate(pop_2022 = parse_number(pop_2022),
         label = round(pop_2022/1e6, 1)) %>% 
  clean_names()

cities$country <- factor(cities$country, levels = unique(cities$country))

# calculate the ANGLE of the labels
number_of_bar <- nrow(cities)
angle <-  90 - 360 * (cities$rank-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
cities$hjust <- ifelse(test = angle < -90, yes = 1, no = 0)
cities$hjust[cities$rank %in% 9:10] <- -0.2

# flip angle BY to make them readable
cities$angle <- ifelse(angle < -90, angle+180, angle)
#cities$angle <- angle


## (3) Creating the plot ----
final_chart <- ggplot(cities, aes(x=as.factor(rank), y=pop_2022, fill=country, label = name)) +       
  geom_bar(stat="identity", color = "black") +
  theme_minimal() +
  ylim(-1e6, 41e6) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(1,0,-4,0), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=cities, 
            aes(x=rank, y=pop_2022+5e5, label= glue('{name}: {label} M'), hjust=hjust), 
            color="black", alpha=0.9, size=3.5, 
            angle= cities$angle, inherit.aes = FALSE ) + 
  labs(fill = "Country: ", title = "Top 20 most populated cities in the world", subtitle = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://worldpopulationreview.com/world-cities")


## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "30_UN_cities2.png", 
       plot = final_chart, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "30_UN_cities2.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 8, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "30_UN_cities2.jpg", 
       plot = final_chart, device = jpeg,  
       width = 8, height = 8, units = "in", dpi = 200)


