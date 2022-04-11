
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse); library(glue); library(patchwork)


## (2) Creating the data ----
twitter <- data.frame(id = 1:10,
                      day = paste0("Day ", 1:10),
                      likes = c(13, 6, 8, 20, 15, 7, 10, 26, 58, 105),
                      category = c("part_to_whole", "pictogram", "historical", "flora", "sloope", "OWID", "physical", "mountains", "statistics", "experimental"),
                      theme = c("MAMMALS", "YTVIDEOS", "POLIOTRIALS", "IRIS", "SIGN", "CHILDMORTALITY", "LIFTS", "EVEREST", "NORM", "PVALUES"))


# label data
# Get the name and the y position of each label
label_data <- twitter

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust <- ifelse(test = angle < -90, yes = 1, no = 0)
label_data$hjust[label_data$id %in% 9:10] <- -0.2

# flip angle BY to make them readable
label_data$angle <- ifelse(angle < -90, angle+180, angle)
#label_data$angle <- angle


## (3) Creating the basic plot ----
basic_plot <- ggplot(twitter, aes(x=as.factor(id), y=likes, fill=as.factor(id), label = day)) +       
  geom_bar(stat="identity", color = "black") +
  ylim(-2,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-3,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, 
            aes(x=id, y=likes+2, label= glue('{day}: {likes} likes'), hjust=hjust), 
            color="black", alpha=0.9, size=3.5, 
            angle= label_data$angle, inherit.aes = FALSE ) 


## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "11_circular_LIKES.png", 
       plot = basic_plot, device = png,  
       width = 7, height = 7, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "11_circular_LIKES.pdf", 
       plot = basic_plot, device = cairo_pdf,  
       width = 7, height = 7, units = "in")

