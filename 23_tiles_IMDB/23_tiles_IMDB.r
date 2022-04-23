
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "rvest", "purrr", "cowplot", "patchwork", "xml2", "RColorBrewer", "magick", "readxl")


## (2) Loading the data and creating a long data format ----
imdb_BrBa <- read_excel(path = "imdb.xlsx",
                        sheet = "imdb_BrBa")

imdb_GoT <- read_excel(path = "imdb.xlsx",
                       sheet = "imdb_GoT")

imdb_BrBa_long <- imdb_BrBa %>% pivot_longer(-Episode) %>% 
  set_names(c("episode", "season", "rating")) %>% 
  mutate(rating = rating/10)

imdb_GoT_long <- imdb_GoT %>% pivot_longer(-Episode) %>% 
  set_names(c("episode", "season", "rating")) %>% 
  mutate(rating = rating/10)


## (3) Creating basic charts ----
# Breaking Bad
bb_basic <- imdb_BrBa_long %>% 
  ggplot(aes(x = season, y = episode, 
             label = rating, fill = rating)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(low = "red", 
                       mid = "orange", high = "darkgreen", 
                       name = "imdb rating", midpoint = 7,
                       limits = c(4,10)) + 
  geom_text(color = "white") +
  scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top") +
  ggtitle("Breaking Bad") +
  labs(x="", y="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "none")

# Game of Thrones
got_basic <- imdb_GoT_long %>% 
  ggplot(aes(x = season, y = episode, 
             label = rating, fill = rating)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(low = "red", 
                       mid = "orange", high = "darkgreen", 
                       name = "imdb rating", midpoint = 7,
                       limits = c(4,10)) + 
  geom_text(color = "white") +
  scale_y_discrete(limits = rev) + 
  scale_x_discrete(position = "top") +
  ggtitle("Game of Thrones") +
  labs(x="", y="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


# combine plots
design <- "
  12
"


combined_chart <-  got_basic + bb_basic +
  patchwork::plot_layout(design = design) 


## (5) Advanced plot with images of show added ----
bb_image <- magick::image_read(path = "bb1.png")
got_image <- magick::image_read(path = "got1.png")


final_chart <- cowplot::ggdraw() + 
  cowplot::draw_plot(combined_chart) + 
  cowplot::draw_image(got_image, hjust = 0, vjust = -0.32, scale = 0.15) + 
  cowplot::draw_image(bb_image, hjust = -0.41, vjust = -0.43, scale = 0.12) 


## (6) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "23_tiles_IMDB.png", 
       plot = final_chart, device = png,  
       width = 10, height = 6, units = "in", dpi = 200)


# save as PDF
ggsave(path = getwd(), filename = "23_tiles_IMDB.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 10, height = 6, units = "in")


