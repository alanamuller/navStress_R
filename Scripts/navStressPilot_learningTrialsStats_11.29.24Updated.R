library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyr)
library(ez)
library(rstatix)
library(ggpubr)

rm(list = ls())

# Set the directory where the Excel files are stored
setwd("D:/Nav Stress Data")

# Number of novel grids in city 1 = 1126

# Input data
learning_route_data <- read_excel("manualCombined_pilot_LearningTrialsOverlap.xlsx")

# fix the structure of the data
learning_route_data$subjectID <- as.factor(learning_route_data$subjectID)
learning_route_data$first_route_learned <- as.factor(learning_route_data$first_route_learned)
learning_route_data$second_route_learned <- as.factor(learning_route_data$second_route_learned)
learning_route_data$outer_reps <- as.factor(learning_route_data$outer_reps)
learning_route_data$inner_reps <- as.factor(learning_route_data$inner_reps)
learning_route_data$more_familiar <- as.factor(learning_route_data$more_familiar)
learning_route_data$less_familiar <- as.factor(learning_route_data$less_familiar)

# rename columns so overlap columns start with outer or inner
learning_route_data <- rename(learning_route_data, outer_overlap = overlap_outer)
learning_route_data <- rename(learning_route_data, inner_overlap = overlap_inner)

# make learning data frame long - columns I want to stay wide
wide_cols <- c("subjectID", "first_route_learned", "second_route_learned")

# make the data long format except for the specified columns
learning_long_data <- pivot_longer(learning_route_data, -wide_cols,
                                   names_to = c("path_type", ".value"), 
                                   names_pattern = "(outer|inner)_(.*)")

# fixing the structure of the long data
learning_long_data$first_route_learned <- as.factor(learning_long_data$first_route_learned)
learning_long_data$second_route_learned <- as.factor(learning_long_data$second_route_learned)
learning_long_data$path_type <- as.factor(learning_long_data$path_type)

# add percent of overlap
learning_long_data$percent_overlap <- (learning_long_data$overlap / learning_long_data$actual_path_grid_tot) * 100

####################### STATS TIME: Learning Phase #######################
# Did participants learn the routes they were supposed to?

overall_percent_overlap <- mean(learning_long_data$percent_overlap)
percent_overlap_by_rep <- learning_long_data %>%
  group_by(reps) %>%
  summarize(
    count = n(), 
    mean_percent_overlap = mean(percent_overlap)
  )

# overlap by reps
boxplot(overlap~reps,data=learning_long_data)

# nav excess by reps ### THIS DOESN'T EXIST IN THE DATA SHEET YET PLEASE ADD IT SOON MAYBE BEFORE SFN 2023
boxplot(overlap~reps,data=learning_long_data)

# overlap by path_type
boxplot(overlap ~ path_type, data = learning_long_data, col = "lightblue")

# 2way ANOVA for path type (inner or outer) and reps (2, 3, 4, 6)
model <- lm(percent_overlap ~ path_type * reps, data = learning_long_data)
anova(model) # nothing sig

interaction.plot(learning_long_data$path_type, learning_long_data$reps, learning_long_data$percent_overlap)

aov_data <- learning_long_data %>%
  group_by(subjectID, path_type, reps) %>%
  summarize(
    mean = mean(percent_overlap, na.rm = TRUE),
  )
aov_data <- as_tibble(aov_data)

# one-way anova
one.way.overlapTest <- anova_test(data = learning_long_data, dv = percent_overlap,
                         between = reps)
get_anova_table(one.way.overlapTest)

# summary stats used in 2way rep ANOVA
aov_means <- learning_long_data %>%
  group_by(path_type, reps) %>%
  get_summary_stats(percent_overlap, type = "mean_sd")

bxp <- ggboxplot(
  aov_data, x = "path_type", y = "mean", 
  color = "reps", add = "jitter",
  xlab = "Path Type", ylab = "Percent Overlap",
  legend = "right", legend.title = "Route Repeats", 
  linetype = 1, size = 1) +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,100,5)) + 
  coord_cartesian(ylim = c(70,100)) +
  scale_x_discrete(labels = c("Inner", "Outer"))

#jpeg("pathType_by_rep.jpeg", width = 8.25, height = 5.75, units = 'in', res = 500)
bxp
#dev.off()


# graph for one-way anova
aov_data2 <- learning_long_data %>%
  group_by(subjectID, reps) %>%
  summarize(
    mean = mean(percent_overlap, na.rm = TRUE),
  )
aov_data2 <- as_tibble(aov_data)

bxp <- ggboxplot(
  aov_data2, x = "reps", y = "mean", add = "jitter",
  xlab = "Number of Learning Trial Pairs", ylab = "Percent Overlap",
  linetype = 1, size = 1) +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,100,5)) + 
  coord_cartesian(ylim = c(70,100))

#jpeg("E:/Nav Stress Data/dissertation/pics/pilot_overlap_reps.jpeg", width = 6, height = 6, units = 'in', res = 500)
bxp
#dev.off()

