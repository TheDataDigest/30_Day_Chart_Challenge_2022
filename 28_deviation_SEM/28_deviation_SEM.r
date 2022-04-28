
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "patchwork")


## (2) Create the data set ----
population_df <- data.frame(x = rnorm(n = 10000, mean = 100, sd = 15))

means_sample_10 <- data.frame(x = rep(NA, 50))
for(i in 1:50) {
  means_sample_10$x[i] <- mean(sample(x = population_df$x, size = 10))
}

sd_10 <- round(sd(means_sample_10$x), 2)

means_sample_40 <- data.frame(x = rep(NA, 50))
for(i in 1:50) {
  means_sample_40$x[i] <- mean(sample(x = population_df$x, size = 40))
}
sd_40 <- round(sd(means_sample_40$x), 2)


# 5 samples for chart
set.seed(28)

# sample size = 10
df_10 <- data.frame(x = sample(x = population_df$x, size = 50),
                    set = rep(LETTERS[1:5], each = 10)) %>% arrange(set, x)
means_10 <- df_10 %>% group_by(set) %>% summarize(mean = mean(x))

sd_10 <- numeric(100)
for(i in 1:100) {sd_10[i] <- mean(sample(x = population_df$x, size = 10))}
sd_10_df <- data.frame(x = sd_10)
  
# sample size = 40
df_40 <- data.frame(x = sample(x = population_df$x, size = 200),
                    set = rep(LETTERS[1:5], each = 40)) %>% arrange(set, x)
means_40 <- df_40 %>% group_by(set) %>% summarize(mean = mean(x))

sd_40 <- numeric(100)
for(i in 1:100) {sd_40[i] <- mean(sample(x = population_df$x, size = 40))}
sd_40_df <- data.frame(x = sd_40)


## (3) Creating the charts ----
pop_chart <- population_df %>% ggplot(aes(x = x)) + 
  geom_histogram(aes(y=..density..), fill = "lightgrey", color = "black") +
  theme_bw() +
  geom_density(data = population_df, aes(x=x), color = "darkgreen", fill = "green", alpha = 0.2, size = 2) + 
  scale_y_continuous(limits = c(0, 0.03)) +
  labs(x="", y="") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("If you want to estimate the mean of a distribution,\nyou have to draw a sample and calculate the sample mean",
          subtitle = "In order do reduce the standard error of the mean by half you have to increase the sample size four times")
  
sample_10_chart <- df_10 %>% ggplot(aes(x = x, color = set, group = set)) + 
  geom_density(size = 1.2, adjust = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(50,150)) + 
  scale_y_continuous(limits = c(0, 0.03)) + 
  geom_vline(data = means_10, aes(xintercept = mean, group = set, color = set), size = 1.5, alpha = 0.8)+
  labs(x="", y="")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Drawing samples with N=10")

sample_40_chart <- df_40 %>% ggplot(aes(x = x, color = set, group = set)) + 
  geom_density(size = 1.2, adjust = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(50,150)) +
  scale_y_continuous(limits = c(0, 0.03)) +
  geom_vline(data = means_40, aes(xintercept = mean, group = set, color = set), size = 1.5, alpha = 0.8)+
  labs(x="", y="")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = "Drawing samples with N=40")

sd_10_chart <- sd_10_df %>% ggplot(aes(x = x)) + 
  geom_histogram(aes(y=..density..), fill = "lightgrey", color = "black") +
  theme_bw() +
  geom_density(data = sd_10_df, aes(x=x), color = "orange", fill = "yellow", alpha = 0.2, size = 1.2) +
  scale_x_continuous(limits = c(50,150)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  ggtitle("SEM: 15/sqrt(10) = 4.74")+
  labs(x="", y="")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  
sd_40_chart <- sd_40_df %>% ggplot(aes(x = x)) + 
  geom_histogram(aes(y=..density..), fill = "lightgrey", color = "black") +
  theme_bw() +
  geom_density(data = sd_40_df, aes(x=x), color = "red", fill = "pink", alpha = 0.2, size = 1.2) +
  scale_x_continuous(limits = c(50,150)) + 
  scale_y_continuous(limits = c(0, 0.2)) +
  ggtitle("SEM: 15/sqrt(40) = 2.37")+
  labs(x="", y="")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


## (4) Combining charts ----
design <- "
  11
  23
  45
"

final_chart <- pop_chart + sample_10_chart + sample_40_chart + sd_10_chart + sd_40_chart +
  patchwork::plot_layout(design = design) + 
  patchwork::plot_annotation(caption = "Visualization: https://www.youtube.com/c/TheDataDigest")


## (5) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "28_deviation_SEM.png", 
       plot = final_chart, device = png,  
       width = 10, height = 8, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "28_deviation_SEM.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 10, height = 8, units = "in", dpi = 200)
