
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

library(easypackages)
libraries("tidyverse", "janitor", "patchwork", "ggpubr", "magick")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## (2) Loading and manipulating the data ----
# use pivot_wider to work with facet_wrap() on multiple measurements
iris_long <- clean_names(iris, case = "title") %>% 
  pivot_longer(-Species) %>% 
  mutate(measurement = factor(name, levels = c("Petal Length", "Sepal Length", "Petal Width",  "Sepal Width")))


## (3) Basic plot ----
# measurement image
p_iris <- iris_long %>% 
  ggplot(aes(x = Species, y = value, 
             fill = Species, color = Species)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(size = 1) +
  facet_wrap(~ measurement, scales = "free_y") +
  theme_bw()

# iris image
iris_image <- magick::image_read(path = "iris_image.jpg")
p_image <- ggplot() + ggpubr::background_image(iris_image)


## (4) Final chart with annotations and layout improvements ----
design <- "
  111
  222
  222
"

final_chart <- p_image + p_iris + 
  labs(x = "", y = "measurements in centimeters") +
  patchwork::plot_layout(design = design)


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "04_flora_IRIS.png", 
       plot = final_chart, device = png,  
       width = 8, height = 8, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "04_flora_IRIS.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 8, units = "in")
