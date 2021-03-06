
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
packages("tidyverse", "readr", "patchwork", "ggtext", "readxl")

install.packages("ggh4x")
library(ggh4x)


## (2) Loading and cleaning data ----
df1 <- readxl::read_excel(path = paste0(getwd(), "/spurious_correlations.xlsx"), sheet = "dataset1")
df2 <- readxl::read_excel(path = paste0(getwd(), "/spurious_correlations.xlsx"), sheet = "dataset2")
df_labels <- readxl::read_excel(path = paste0(getwd(), "/spurious_correlations.xlsx"), sheet = "labels")

names(df1)[3:4] <- c(df_labels$unit1[1], df_labels$unit2[1])

cor_1 <- as.character(round(cor.test(df1$`billion dollar`, df1$suicides)$estimate, 4))


## (3) Basic plot ----
# Run the secondary axis helper
sec <- ggh4x::help_secondary(df1, primary = `billion dollar` , secondary = suicides)

# Making primary plot
p1 <- ggplot(df1, aes(x = year)) +
  geom_line(aes(y = `billion dollar`), colour = "black") + 
  geom_point(aes(y = `billion dollar`), colour = "black")

# For the secondary data, later we use the `proj` function from the helper
p2 <- p1 + geom_line(aes(y = sec$proj(suicides)), colour = "red") +
  geom_point(aes(y = sec$proj(suicides)), colour = "red")

# We feed the scale the secondary axis
basic_1 <- p2 + scale_y_continuous(sec.axis = sec) + theme(axis.title.y.left = element_text(color = "red"))


## (4) Improve image theme and annotations ----
final_chart <- basic_1 + 
  theme_bw() + 
  xlab(label = "") + 
  scale_x_continuous(breaks = 1999:2009)  + 
  theme(axis.title.y.left = element_text(color = "red"),
        axis.text.y.left = element_text(color = "red"),
        axis.ticks.y.left = element_line(color = "red")) + 
  patchwork::plot_annotation(
    title = paste0("<b style='color:#fc0d0d'>",df_labels$title1[1],"</b>",
                   "<br><b style='color:#808080; font-size: 11pt'>correlates with</b>", 
                   "<br><b style='color:#0a0a0a'>",df_labels$title2[1],"</b>"),
    subtitle = paste0("Correlation: r=", cor_1),
    caption = 'Visualization: https://www.youtube.com/c/TheDataDigest\nData source: https://www.tylervigen.com/spurious-correlations',
    theme = theme(plot.title = ggtext::element_markdown(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5)))


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "13_correlation_SPURIOUS.png", 
       plot = final_chart, device = png,  
       width = 7, height = 4, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "13_correlation_SPURIOUS.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 7, height = 4, units = "in")

