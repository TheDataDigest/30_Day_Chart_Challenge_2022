
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

library(easypackages)
packages("tidyverse", "readr", "lubridate", "ggimage", "ggtext", "zoo", "readxl")

Sys.setenv(LANG = "en")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## (2) Loading and cleaning data ----
uploads <- readxl::read_excel(path = "uploads.xlsx") %>% 
  mutate(image = "YT_logo.png",
         date = lubridate::mdy(date),
         month_year = zoo::as.yearmon(date, "%m/%Y"))

## (3) Calculate, summarize, estimate etc. ----
month_results <- uploads %>% count(month_year)

df <- data.frame(date = as.Date(character()),
                 image = character(), 
                 y_value = integer(), 
                 stringsAsFactors=FALSE) 

for(i in 1:nrow(month_results)) {
  temp_df <- data.frame(y_value = 1:month_results$n[i]) %>% 
    mutate(date = as.Date(month_results$month_year[i]),
           image = "YT_logo.png")
  
  df <- bind_rows(df, temp_df)
}

## (4) Final plot with layout improvement ----

final_chart <- ggplot(
  data = df %>% group_by(date) %>% summarize(y_value = max(y_value)),
  aes(x = date, y = y_value)) +
  geom_col(fill = "white", color = "white") + 
  ggimage::geom_image(data = df,
             aes(x = date, y = y_value - 0.5, image = image)) +
  #  theme_dark() + 
  labs(x = "", y = "Number of uploads",
       caption = "Visualization: https://www.youtube.com/c/TheDataDigest  |  Day 2: Pictogram") +
  ggtext::geom_richtext(aes(x = as.Date("2021-11-01"), y = 6.6,
                    label = "<b style='color:#000000;font-size:30pt;'>My YouTube Journey</b><br>
                            <b style='color:#000000;font-size:20pt;'>25 Videos in 16 Months</b></span>"),
                lineheight = 3, family = "Work Sans", vjust = 0.5,
                stat = "unique", fill = "#ffffff", label.color = "black") + 
  theme_void() +
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%Y") +
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "grey15"),
    axis.title.y = element_text(color = "grey80", size = 14, face = "bold", angle = 90),
    axis.text.x = element_text(color = "grey80", size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "grey50", linetype = "13", size = .4),
    plot.margin = margin(15, 30, 10, 30),
    plot.caption = element_text(family = "Work Sans", color = "white", size = 10,
                                margin = margin(t = 30, b = 0)),
    legend.position = "none"
)


## (5) Save png, pdf ----
# save as PNG
ggsave(path = here::here(), filename = "02_pictogram_YTVIDEOS.png", 
       plot = final_chart, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = here::here(), filename = "02_pictogram_YTVIDEOS.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 8, units = "in")
