## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData

# Data reference:
# https://www.tylervigen.com/spurious-correlations

## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
packages("tidyverse", "readr", "patchwork", "ggtext", "readxl", "ggh4x", "janitor")


## (2) Loading and cleaning data ----
twitter_df <- readxl::read_excel(path = paste0(getwd(), "/twitter.xlsx"), sheet = 1) %>% 
  clean_names()
  



## (3) Basic plot ----
# Run the secondary axis helper
sec <- ggh4x::help_secondary(twitter_df, primary = likes , secondary = followers)


p1 + geom_line(aes(x = day, y = followers), color = "darkred")


# Making primary plot
p1 <- ggplot(twitter_df, aes(x = day, label = likes)) +
  geom_col(aes(y = likes, fill = likes), colour = "black") + 
  geom_text(aes(y = likes + 5), size = 3, colour = "black") +
  theme_bw() +
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p2 <- p1 + geom_line(aes(y = sec$proj(followers)), colour = "darkred", size = 1.1) +
  geom_point(aes(y = sec$proj(followers)), colour = "darkred", size = 2)

# We feed the scale the secondary axis
basic_1 <- p2 + scale_y_continuous(sec.axis = sec)


## (4) Improve image theme and annotations ----
final_chart <- basic_1 + 
  theme_bw() + 
  xlab(label = "Day") + 
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30)) +
  theme(axis.title.y.left = element_text(color = "darkblue"),
        axis.text.y.left = element_text(color = "darkblue"),
        axis.ticks.y.left = element_line(color = "darkblue"),
        axis.title.y.right = element_text(color = "darkred"),
        axis.text.y.right = element_text(color = "darkred"),
        axis.ticks.y.right = element_line(color = "darkred"),
        legend.position = "none") + 
  patchwork::plot_annotation(
    title = paste0("Thanks to every retweet and <b style='color:#00008b'>like</b> and",
                   "<br>everyone who <b style='color:#8b0000'>followed</b> me during the", 
                   "<br> #30DayChartChallenge"),
    #subtitle = paste0("Correlation: r=", cor_1),
    theme = theme(plot.title = ggtext::element_markdown(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5)))


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "31_TWITTER.png", 
       plot = final_chart, device = png,  
       width = 8, height = 4, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "31_TWITTER.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 4, units = "in")
