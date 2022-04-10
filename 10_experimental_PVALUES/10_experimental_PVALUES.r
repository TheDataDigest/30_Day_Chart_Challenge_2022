
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData

## Tutorial references
# https://cran.r-project.org/web/packages/ggprism/vignettes/pvalues.html
# based on tutorial by Alboukadel Kassambara @datanovia
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install.packages("rstatix"); install.packages("ggprism")
library(tidyverse)
library(ggprism)
library(rstatix)
library(patchwork)


## (2) Loading the data and calculating p-values ----
df_p_val1 <- ToothGrowth %>%
  rstatix::group_by(dose) %>%
  rstatix::t_test(len ~ supp) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "dose", dodge = 0.8) # important for positioning!

df_p_val2 <- rstatix::t_test(ToothGrowth, len ~ dose, 
                             p.adjust.method = "bonferroni") %>% 
  rstatix::add_xy_position()


## (3) Creating the basic plot ----
# basic plot
basic_plot <- ToothGrowth %>% ggplot(aes(x = factor(dose), y = len)) + 
  geom_boxplot(aes(fill = supp), alpha = 0.4) + 
  geom_point(aes(x = factor(dose), y = len, color = supp), 
             position=position_jitterdodge(), alpha = 0.6) +
  theme_prism() + 
  coord_cartesian(ylim = c(0, 45)) +
  theme(legend.position = "top")

# changing the theme
basic_plot <- ggpubr::set_palette(basic_plot, "jco") +
  labs(x = "Dose in milligrams/day" , y = "Tooth length in mm")

# adding p-values
annotated_plot <- basic_plot + 
  add_pvalue(df_p_val1, xmin = "xmin", xmax = "xmax",
               label = "p = {p.adj}", tip.length = 0) + 
  add_pvalue(df_p_val2, label = "p = {p.adj}", 
             tip.length = 0.01, bracket.nudge.y = 2, step.increase = 0.015)


## (4) Create final chart with annotations ----
final_chart <-  annotated_plot +
  patchwork::plot_annotation(
    title = "The Effect of Vitamin C on Tooth Growth in Guinea Pigs",
    subtitle = "The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs.\nEach animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day)\nby one of two delivery methods: orange juice (OJ) or ascorbic acid (VC).", 
    caption = "Visualization: https://www.youtube.com/c/TheDataDigest | Data source: datasets::ToothGrowth\nReference: Crampton, E. W. (1947): The Journal of Nutrition, 33(5), 491–504. doi: 10.1093/jn/33.5.491.\nThe growth of the odontoblast of the incisor teeth as a criterion of vitamin C intake of the guinea pig.")


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "10_experimental_PVALUES.png", 
       plot = final_chart, device = png,  
       width = 7, height = 7, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "10_experimental_PVALUES.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 7, height = 7, units = "in")
