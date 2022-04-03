
## 30 day chart challenge 2022 ##

# Script by TheDataDigest 
# https://www.youtube.com/c/TheDataDigest
# https://github.com/TheDataDigest
# https://twitter.com/DigestData


## (1) Setup and loading packages ----
rm(list=ls())

install.packages("easypackages"); library(easypackages)
libraries("HistData", "tidyverse", "ggthemes", "janitor", "snakecase", "glue", "ggtext")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## (2) Loading and cleaning data ----
data("PolioTrials")

polio_data <- PolioTrials %>% 
  janitor::clean_names() %>% 
  filter(group != "IncompleteVaccinations") %>% 
  select(-false_reports)

?PolioTrials
## Background
# The data frame is in the form of a single table, but actually comprises the results of two separate field trials, given by Experiment. Each should be analyzed separately, because the designs differ markedly.
# The original design (Experiment == "ObservedControl") called for vaccination of second-graders at selected schools in selected areas of the country (with the consent of the children's parents, of course). The Vaccinated second-graders formed the treatment group. The first and third-graders at the schools were not given the vaccination, and formed the Controls group.
# In the second design (Experiment == "RandomizedControl") children were selected (again in various schools in various areas), all of whose parents consented to vaccination. The sample was randomly divided into treatment (Group == "Vaccinated"), given the real polio vaccination, and control groups (Group == "Placebo"), a placebo dose that looked just like the real vaccine. The experiment was also double blind: neither the parents of a child in the study nor the doctors treating the child knew which group the child belonged to.
# In both experiments, NotInnoculated refers to children who did not participate in the experiment.
# IncompleteVaccinations refers to children who received one or two, but not all three administrations of the vaccine.

## Source
# Kyle Siegrist, "Virtual Laboratories in Probability and Statistics", http://www.math.uah.edu/stat/data/Polio.html
# Thomas Francis, Robert Korn, et al. (1955). "An Evaluation of the 1954 Poliomyelitis Vaccine Trials", American Journal of Public Health, 45, (50 page supplement with a 63 page appendix).

## References
# K. A. Brownlee (1955). "Statistics of the 1954 Polio Vaccine Trials", Journal of the American Statistical Association, 50, 1005-1013.


## (3) Calculations and data manipulation ----
polio_long <- polio_data %>% 
  pivot_longer(cols = -c(experiment:population)) %>% 
  mutate(cases_per_100k = round(value/population * 100000, 1)) %>% 
  mutate_if(.predicate = is.factor, .funs = as.character) 

# apply() to turn character from CamelCase to Title
polio_long[, c("experiment", "group", "name")] <- apply(polio_long[c("experiment", "group", "name")], MAR = 2, FUN = function(x) {snakecase::to_any_case(x, case = "title")})

polio_long <- polio_long %>% 
  mutate(name = factor(name, levels = c("Paralytic", "Non Paralytic")),
         group = factor(group, levels = c("Vaccinated", "Grade 2 not Inoculated", "Controls", "Not Inoculated", "Placebo"), ordered = TRUE),
         group_N = glue("{group}\n(N = {round(population/1000,0)}k)"))

# reorder data based on group levels and turn group_N into new factor to keep order and use it as x-axis label
polio_long <- polio_long[order(polio_long$group), ]
polio_long$group_N <- factor(polio_long$group_N, levels = unique(polio_long$group_N), ordered = TRUE)


## (4) Basic plot ----
p1 <- polio_long %>% ggplot(aes(x = group_N, y = cases_per_100k, fill = name, label = cases_per_100k)) +
  geom_col(position = "stack", color = "black") + 
  facet_wrap(~experiment, scales = "free_x") +
  geom_text(position = position_stack(vjust = .5)) # center in stacked segment


## (5) Final chart with annotations and layout improvements ----
final_chart <- p1 + theme_solarized() + scale_fill_manual(values = c("#66c2a5", "#d8b365")) +
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11),
        plot.subtitle = ggtext::element_markdown()) +
  labs(title = "Results of the 1954 field trials to test the Salk polio vaccine", 
       fill = "Polio symptoms: ",
       subtitle = "They showed ~65% effectiveness to prevent <b style='color:#66c2a5'>paralytic polio symptoms</b> and <br>were conducted by the National Foundation for Infantile Paralysis (NFIP)",
       x = "", y = "Cases per 100k",
       caption = '#30DayChartChallenge Day 3: Historical | Visualization: https://www.youtube.com/c/TheDataDigest | Data: HistData::PolioTrials
       \nSource: Thomas Francis, Robert Korn, et al. (1955). "An Evaluation of the 1954 Poliomyelitis Vaccine Trials", American Journal of Public Health, 45\nReferences: K. A. Brownlee (1955). "Statistics of the 1954 Polio Vaccine Trials", Journal of the American Statistical Association, 50, 1005-1013')


## (6) Save png, pdf ----
# save as PNG
ggsave(path = getwd(), filename = "03_historical_POLIOTRIALS.png", 
       plot = final_chart, device = png,  
       width = 9, height = 9, units = "in", dpi = 200)

# save as PDF
ggsave(path = getwd(), filename = "03_historical_POLIOTRIALS.pdf", 
       plot = final_chart, device = cairo_pdf,  
       width = 9, height = 9, units = "in")


