
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(easypackages)
easypackages::libraries("tidyverse", "scales")


## (2) Create the data set ----
rates <- c(1, 1.02, 1.04, 1.06, 1.08, 1.10)
year <- 1:20

money <- matrix(NA, nrow = 20, ncol = length(rates))

for (j in 1:length(rates)) {
  i <- 2
  money[1,j] <- 1200
  while (i <= 20) {
    money[i,j] <- money[i-1,j] * rates[j] + 1200
    i <- i + 1
  }
}

money_df <- as.data.frame(money)
names(money_df) <- rates
money_df$year <- row.names(money_df)

money_df_long <- pivot_longer(money_df, cols = -year)
money_df_long$year <- as.numeric(money_df_long$year)

money_df_long <- money_df_long %>% arrange(desc(name))
money_df_long <- money_df_long %>% mutate(
  category = case_when(
    name == "1" ~ "0%",
    name == "1.02" ~ "2%",
    name == "1.04" ~ "4%",
    name == "1.06" ~ "6%",
    name == "1.08" ~ "8%",
    name == "1.1" ~ "10%"
  )
)

money_df_long$category <- factor(x = money_df_long$category, 
                      levels = c("10%", "8%", "6%", "4%", "2%", "0%"), ordered = TRUE)


## (3) Creating final chart ----
final_chart <- money_df_long %>% ggplot(aes(x = year, y = value, fill = category)) +
  geom_area(position = "dodge", alpha = 0.6, color = "black") + 
  theme_linedraw() +
  scale_y_continuous(labels = dollar_format()) + 
  labs(x = "Time in years", y = "", fill = "Growth rate:", 
       title = "If you save $100 per month for 20 years ...",
       subtitle = "... this is how much it would accrue to based on different growth rates.")



## (4) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "27_future_GROWTH.png", 
       plot = final_chart, device = png,  
       width = 8, height = 4, units = "in", dpi = 200)

ggsave(path = getwd(), filename = "27_future_GROWTH.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 8, height = 4, units = "in", dpi = 200)


