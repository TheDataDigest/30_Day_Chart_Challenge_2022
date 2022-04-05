
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

library(tidyverse)
#library(remotes)
#remotes::install_github("AllanCameron/geomtextpath")
#install.packages("geomtextpath")
library(geomtextpath); library(magick); library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## (2) Creating the data data ----
slope_df <- data.frame(x = rep(x = c(0, 100), 6),
                       y = c(0, 5, 0, 8, 0, 10, 0, 15, 0, 20, 0, 25),
                       percent = c(5, 5, 8, 8, 10, 10, 15, 15, 20, 20, 25, 25)) %>% 
  mutate(angle = atan(percent/100) * 180 / pi,
         category = paste0("Slope: ",percent, "%"),
         label = paste0(percent, "% = ", round(angle,1), "° angle"))

# turn category into factor with increasing levels of percent/angle
slope_df$category <- factor(slope_df$category, levels = unique(slope_df$category), ordered = TRUE)


## (3) Basic plot ----
p_slope <- slope_df %>% 
  ggplot(aes(x = x, y = y, label = label)) + 
  xlim(c(0,100)) + ylim(c(0,25)) +
  geom_area(fill = "#fee111") + 
  geom_curve(aes(x = 0, y = 0, xend = 100, yend = percent), curvature = 0) +
  geomtextpath::geom_labelcurve(aes(x = 0, y = 0, xend = 100, yend = percent), vjust = -0.5, curvature = 0) +
  facet_wrap(~ category, ncol = 2) +
  coord_fixed() +
  theme_bw() + 
  theme(strip.text.x = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "", y = "")


## (4) Final chart with with added road sign ----
sign_image <- magick::image_read(path = "slope.png")

final_chart <- cowplot::ggdraw() + 
  cowplot::draw_plot(p_slope) + 
  cowplot::draw_image(sign_image, hjust = 0.4, vjust = -0.268, scale = 0.15)


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "05_slope_SIGN.png", 
       plot = final_chart, device = png,  
       width = 8, height = 5, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "05_slope_SIGN.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 5, units = "in")


