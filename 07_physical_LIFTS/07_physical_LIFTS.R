

## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "rvest", "purrr", "cowplot", "patchwork", "xml2", "RColorBrewer", "magick")


## (2) Loading the data ----
url_kg <- "https://strengthlevel.com/strength-standards/male/kg" 
url_lb <- "https://strengthlevel.com/strength-standards/male/lb" 

raw_list <- url_kg %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("table") %>% 
  rvest::html_table(fill = T) 

# fixing the names of the different list entries
raw_list <- raw_list[1:7] %>% 
  lapply(X = ., FUN = function(x) {setNames(x, c("Bodyweight", "Beginner", "Novice", "Intermediate", "Advanced", "Elite"))})

tables <- data.frame()
lifts <- c("Bench Press", "Squat", "Deadlift", "Shoulder Press", "Barbell Curl", "Bent Over Row", "Power Clean")

# turn lists into a data.frame
for (i in 1:7) {
  temp <- raw_list[[i]]
  temp$lift <- lifts[i]
  tables <- bind_rows(tables, temp)
}


## (3) Manipulating the data, preparation for the plot ----
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


## (4) Basic plot ----

# kilogram for barbell weight and bodyweight
p_lifts_kg <- lifts_df %>% 
  filter(lift %in% c("Bench Press", "Deadlift", "Squat")) %>% 
  ggplot(aes(x = bodyweight_kg, y = barbell_kg, color = level)) +
  geom_line(size = 1.2) + 
  scale_colour_manual(values = brewer.pal(5, "Paired")) +
  facet_wrap(~lift, ncol = 2) +
  theme_linedraw() +
  guides(color = guide_legend(reverse = TRUE)) + 
  labs(title = "Strength levels for Bench press, Deadlift and Squat", 
       subtitle = "Which weight (y-axis) do you have to lift given your bodyweight (x-axis)\nto achieve a certain level of lifters (Beginner - Elite)", 
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://strengthlevel.com/strength-standards/male/kg", 
       x = "Bodyweight [kg]", y = "Barbell weight [kg]", color = "Level of lifter:") +
  theme(legend.position = c(0.75, 0.25))

# lbs (pounds) instead of kilogram for barbell weight and bodyweight
p_lifts_lb <- lifts_df %>% 
  filter(lift %in% c("Bench Press", "Deadlift", "Squat")) %>% 
  ggplot(aes(x = bodyweight_lb, y = barbell_lb, color = level)) +
  geom_line(size = 1.2) + 
  scale_colour_manual(values = brewer.pal(5, "Paired")) +
  facet_wrap(~lift, ncol = 2) +
  theme_linedraw() +
  guides(color = guide_legend(reverse = TRUE)) + 
  labs(title = "Strength levels for Bench press, Deadlift and Squat", 
       subtitle = "Which weight (y-axis) do you have to lift given your bodyweight (x-axis)\nto achieve a certain level of lifters (Beginner - Elite)", 
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://strengthlevel.com/strength-standards/male/lb", 
       x = "Bodyweight [lbs]", y = "Barbell weight [lbs]", color = "Level of lifter:") +
  theme(legend.position = c(0.75, 0.25))

## (5) Advanced plot with images of lifts added ----
bench_image <- magick::image_read(path = "bench.png")
squat_image <- magick::image_read(path = "squat.png")
deadlift_image <- magick::image_read(path = "deadlift.png")

final_chart_kg <- cowplot::ggdraw() + 
  cowplot::draw_plot(p_lifts_kg) + 
  cowplot::draw_image(bench_image, hjust = 0.35, vjust = -0.27, scale = 0.12) + 
  cowplot::draw_image(deadlift_image, hjust = -0.10, vjust = -0.27, scale = 0.12) +
  cowplot::draw_image(squat_image, hjust = 0.35, vjust = 0.1, scale = 0.12)

final_chart_lb <- cowplot::ggdraw() + 
  cowplot::draw_plot(p_lifts_lb) + 
  cowplot::draw_image(bench_image, hjust = 0.35, vjust = -0.27, scale = 0.12) + 
  cowplot::draw_image(deadlift_image, hjust = -0.10, vjust = -0.27, scale = 0.12) +
  cowplot::draw_image(squat_image, hjust = 0.35, vjust = 0.1, scale = 0.12)


## (6) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "07_physical_LIFTS_kg.png", 
       plot = final_chart_kg, device = png,  
       width = 7, height = 7, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "07_physical_LIFTS_lbs.png", 
       plot = final_chart_lb, device = png,  
       width = 7, height = 7, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "07_physical_LIFTS_kg.pdf", 
       plot = final_chart_kg, device = cairo_pdf,  
       width = 7, height = 7, units = "in")

ggsave(path = getwd(), filename = "07_physical_LIFTS_lbs.pdf", 
       plot = final_chart_lb, device = cairo_pdf,  
       width = 7, height = 7, units = "in")
