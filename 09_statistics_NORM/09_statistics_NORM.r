

## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse); library(scales); library(patchwork)

options(digits = 3)
#theme_set(theme_bw())
theme_set(theme_classic())

## (2) Creating the data ----

set.seed(1)
rnorm_df <- data.frame(r = rnorm(200, mean = 100, sd = 15), value = 1:200, box = 100)

polygon_data <- data.frame(
  x = c(50, seq(from = 50, to = 150, by = 0.5), 150),
  y = c(0, dnorm(seq(from = 50, to = 150, by = 0.5), mean = 100, sd = 15), 0))

## (3) Creating the basic plots ----
# 3.1) rnorm
r_plot <- rnorm_df %>% ggplot() + 
  geom_boxplot(aes(x = box, y = r), 
               width = 50, alpha = 0.3, fill =  "#9A3A3A", color = "#9A3A3A") +
  geom_point(aes(x = value, y = r), 
             size = 2, alpha = 0.7, color = "#9A3A3A") + 
  labs(x = "", y = "") + ggtitle(label = "rnorm(n = 200, mean = 100, sd = 15)", 
                                 subtitle = "generates random data")

# 3.2) dnorm
d_plot <- data.frame(x = 50:150, y = dnorm(x = 50:150, mean = 100, sd = 15)) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_polygon(data = polygon_data, 
                 aes(x = x, y = y),
                 fill = "#4472C4", 
                 alpha = 0.3) +
  geom_line(size = 2, 
            color = "#4472C4") +
  labs(x = "", y = "") +
  ggtitle(label = "dnorm()", subtitle = "creates the probability density function (PDF), \nwhere the area under the curve sums up to 100%")

# 3.3) pnorm
pnorm_x = 125
pnorm_y = pnorm(q = pnorm_x, mean = 100, sd = 15)

p_plot <- data.frame(x = 50:150, y = pnorm(q = 50:150, mean = 100, sd = 15)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 2, 
            color = "#70AD47") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent, breaks = c(0, .25, .5, .75, pnorm_y, 1)) + 
  ggtitle(label = "pnorm()", subtitle = "tells you that with an IQ of 125\nyou are at the top 95.2%") +
  geom_segment(x = pnorm_x, xend = pnorm_x, y = -0.1, yend = pnorm_y, linetype = "dashed") + 
  geom_segment(x = 40, xend = pnorm_x, y = pnorm_y, yend = pnorm_y, linetype = "dashed")
  

# 3.4) qnorm
qnorm_x = 0.85
qnorm_y = qnorm(p = qnorm_x, mean = 100, sd = 15)

q_plot <- data.frame(x = seq(from = 0, to = 1, by = 0.005), 
                     y = qnorm(p = seq(from = 0, to = 1, by = 0.005), mean = 100, sd = 15)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 2, 
            color = "#ED7D31") +
  labs(x = "", y = "") + 
  scale_x_continuous(labels = scales::percent, breaks = c(0, .25, .5, .75, qnorm_x, 1)) + ggtitle("qnorm()") +
  scale_y_continuous(breaks = c(50, 75, 100, qnorm_y, 125, 150), limits = c(50, 150)) + 
  ggtitle(label = "qnorm()", subtitle = "tells you that if you want to be in the top 85%\nyou need an IQ of 116") +
  geom_segment(x = qnorm_x, xend = qnorm_x, y = 40, yend = qnorm_y, linetype = "dashed") + 
  geom_segment(x = -0.1, xend = qnorm_x, y = qnorm_y, yend = qnorm_y, linetype = "dashed")




## (4) Combine plots with patchwork ----

design <- "
  12
  34
"

final_chart <-  r_plot + d_plot + p_plot + q_plot +
  patchwork::plot_layout(design = design) + 
  patchwork::plot_annotation(
    title = "Different ways to use the probability functions in R for the normal distribution",
    subtitle = "Imagine the normal distribution of the Intelligence Quotients (IQ) with mean = 100 and sd = 15.", 
    caption = "Visualization: https://www.youtube.com/c/TheDataDigest")

## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "09_statistics_NORM.png", 
       plot = final_chart, device = png,  
       width = 8.5, height = 8.5, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "09_statistics_NORM.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8.5, height = 8.5, units = "in")
