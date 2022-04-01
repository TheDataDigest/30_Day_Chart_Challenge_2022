
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

install.packages("easypackages")
library(easypackages)
packages("tidyverse", "readr", "paletteer", "magick", "ggpubr", "patchwork", "ggtext")
  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## (2) Loading and cleaning data ----
mammals_raw <- readr::read_delim(file = "mammals_data.csv", delim = ";")


## (3) Calculate, summarize, estimate etc. ----
mammals <- mammals_raw %>%
  group_by(superorder) %>% 
  add_count(wt = species) %>% 
  ungroup() %>% 
  mutate(new_order = ifelse(superorder %in% c("Afrotheria", "Xenarthra"), superorder, order),
         new_order = ifelse(is.na(superorder), "Marsupialia", new_order), 
         new_species = ifelse(new_order %in% c("Afrotheria", "Xenarthra", "Marsupialia"), n, species)) %>% 
  select(infraclass, superorder, new_order, new_species) %>% 
  distinct() %>% 
  rename(species = new_species, order = new_order)
  
mammals <- mammals %>%  
  mutate(fraction = species / sum(species),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)),
         label_position = (ymax + ymin) / 2,
         label_percent = fraction,
         label_order = paste0(order, "\n(", species," | ", round(fraction * 100, 1), " %)"),
         label_order2 = paste0(order, " (", species," | ", round(fraction * 100, 1), " %)")) %>% 
  mutate(order = factor(order, levels = order))

superorder_sum <- mammals %>% 
  group_by(superorder) %>% count(wt = species) %>% ungroup() %>% 
  mutate(percent = round(n/sum(n) * 100, 1),
         label = paste0(superorder, " (", n, " | ", percent, " %"))

total_species <- sum(mammals$species)


## (4) Basic plot ----
coloring <- c("#8E2043FF", 
              paletteer::paletteer_d("colorBlindness::LightBlue2DarkBlue7Steps", n = 7)[c(6:3,7)],
              paletteer::paletteer_d("rcartocolor::Peach")[7:2],
              "#7AD151FF",
              "#6F8BA0FF")
              
donut <- mammals %>%
  ggplot(aes(ymax=ymax, ymin=ymin, 
             xmax=6, xmin=12, 
             fill = order)) +
  geom_rect(alpha = 0.9, color = "black") +
  coord_polar(theta="y") +
  xlim(c(0, 13)) +
  ylim(c(0, 1)) +
  theme_void() +
  theme(legend.position = "none") + 
  # geom_text(data = filter(mammals, fraction > 0.04),
  #           aes(x = 10, y = label_position, 
  #               label = order), 
  #           size = 3.5) + 
  scale_fill_manual(values = coloring) 


## (5) Generate image charts as background_image() ----
# urls
url_rodentia <- "https://upload.wikimedia.org/wikipedia/commons/4/40/Rodent_collage.jpg"
url_primates <- "https://upload.wikimedia.org/wikipedia/commons/9/97/Primates_-_some_families.jpg"
url_chiroptera <- "https://upload.wikimedia.org/wikipedia/commons/f/fd/Wikipedia-Bats-001-v01.jpg"
url_artiodactyla <- "https://upload.wikimedia.org/wikipedia/commons/1/1f/The_Artiodactyla.jpg"
url_eulipotyphla <- "https://upload.wikimedia.org/wikipedia/commons/5/56/Eulipotyphla.jpg"
url_carnivora <- "https://upload.wikimedia.org/wikipedia/commons/4/43/Carnivora_portraits.jpg"
url_marsupialia <- "https://upload.wikimedia.org/wikipedia/commons/6/6a/Marsupialia.jpg"

# prepare loop elements/vectors
order_names <- c("rodentia", "primates", "chiroptera", "artiodactyla", "eulipotyphla", "carnivora", "marsupialia")
urls <- c(url_rodentia, url_primates, url_chiroptera, url_artiodactyla, url_eulipotyphla, url_carnivora, url_marsupialia)
mammal_labels <- mammals$label_order2[c(2,3,7,8,9,10,14)]
loop_colors <- coloring[c(2,3,7,8,9,10,14)]

# loop with assign() function to create objects inside the loop
for(i in 1:7) {
  
  temp_image <- magick::image_read(urls[i])
  
  if(order_names[i] %in% c("primates", "carnivora")){
  temp_plot <- ggplot() + ggpubr::background_image(temp_image) +
    ggtitle(mammal_labels[i]) +
    theme(panel.border = element_rect(color = loop_colors[i], fill = NA, size = 3)) + 
    theme(plot.title = element_text(hjust = 0.5))
  }
  
  if(!order_names[i] %in% c("primates", "carnivora")){
  temp_plot <- ggplot() + ggpubr::background_image(temp_image) + coord_fixed() + 
    ggtitle(mammal_labels[i]) +
    theme(panel.border = element_rect(color = loop_colors[i], fill = NA, size = 3)) + 
    theme(plot.title = element_text(hjust = 0.5))
  }
  
  assign(paste0("p_", order_names[i]), temp_plot)
}


## (6) Combine plots with plot_layout(design) ----
design <- "
  12568
  13478
"

final_chart <-  p_carnivora + p_eulipotyphla + p_artiodactyla + p_chiroptera + donut + 
  p_marsupialia + p_rodentia + p_primates + 
  patchwork::plot_layout(design = design) + 
  patchwork::plot_annotation(
    title = paste0('There are ~', total_species, ' mammalian species that fall within 26 different orders'),
    subtitle = paste0("The biggest two orders are ", "<b style='color:#00AACCFF'>rodents (39%)</b>"," and ", "<b style='color:#EB4A40FF'>bats (21%)</b>"),
    caption = 'Visualization: https://www.youtube.com/c/TheDataDigest\nData source: How many species of mammals are there? (Upham 2018) https://academic.oup.com/jmammal/article/99/1/1/4834091\nImage source: https://www.wikipedia.org/',
    theme = theme(plot.subtitle = ggtext::element_markdown()))


## (7) Save png, pdf ----
# save as PNG
ggsave(path = here::here(), filename = "01_part_to_whole_MAMMALS.png", 
       plot = final_chart, device = png,  
       width = 16, height = 9, units = "in", dpi = 200)

# save as PDF
ggsave(path = here::here(), filename = "01_part_to_whole_MAMMALS.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 16, height = 9, units = "in")