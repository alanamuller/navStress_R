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
setwd("E:/Nav Stress Data")

learning_route_data <- read_excel("learned_trials_overlap_repetitions.xlsx", sheet = "Sheet1")

# fix the structure of the data
learning_route_data$subjectID <- as.factor(learning_route_data$subjectID)
learning_route_data$overlap_outer <- as.numeric(learning_route_data$overlap_outer)
learning_route_data$overlap_inner <- as.numeric(learning_route_data$overlap_inner)
learning_route_data$outer_reps <- as.factor(learning_route_data$outer_reps)
learning_route_data$inner_reps <- as.factor(learning_route_data$inner_reps)

# rename columns so overlap columns start with outer or inner
learning_route_data <- rename(learning_route_data, outer_overlap = overlap_outer)
learning_route_data <- rename(learning_route_data, inner_overlap = overlap_inner)

# Set working directory again to grab more files
setwd("E:/Nav Stress Data/navTestTrials_datasheets")

# Get a list of all the Excel files in the directory
excel_files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

# Make a data frame list
excel_data_list <- list()

# Loop through the Excel files and read them into R
for (i in 1:length(excel_files)) {
  file_name <- excel_files[i]
  sheet_name <- "Sheet 1"  # Replace with the name of your sheet if it's different
  excel_data_list[[i]] <- read_excel(file_name, sheet = sheet_name)
}

# make one big data frame with all the data from all participants
navTestTrials <- do.call(rbind, excel_data_list)
navTestTrials <- navTestTrials[!navTestTrials$trialType == FALSE, ]

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

# uncomment this to save manuscript-quality pics to this folder
#setwd("E:/Nav Stress Data/pics/SFN2023")

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

######################### Navigation Test Trials #########################

# Dataframe with only relevant info for navTrials
sm_navTestTrials_path <- navTestTrials[, c(1, 4, 7:9, 18)]
sm_navTestTrials_df <- navTestTrials[, c(1, 11:14, 18)]

# Melt data frame into long format
wide_cols <- c("subjectID", "trialType")
sm_navTestTrials_long <- melt(sm_navTestTrials_df, id.vars = wide_cols, variable.name = "DV", value.name = "Value")

# boxplot of just DV and values
bxp <- ggplot(sm_navTestTrials_long, aes(x = DV, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), 
        axis.text.y = element_text(size = 14))
#jpeg("overlap.jpeg", width = 5, height = 5, units = 'in', res = 500)
bxp
#dev.off()

# Create faceted plot for overlap by trial type
bxp <- ggplot(sm_navTestTrials_long, aes(x = DV, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ trialType, scales = "free_y", ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 14))
#jpeg("overlap_trialType.jpeg", width = 5, height = 5, units = 'in', res = 500)
bxp
#dev.off()



# Melt data frame into long format
wide_cols <- c("subjectID", "trial", "trialType")
sm_navTestTrials_path_long <- melt(sm_navTestTrials_path, id.vars = wide_cols, variable.name = "DV", value.name = "Value")

# boxplot of just DV and values
bxp <- ggplot(sm_navTestTrials_path_long, aes(x = DV, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), 
        axis.text.y = element_text(size = 14))
#jpeg("path_variables.jpeg", width = 5, height = 5, units = 'in', res = 500)
bxp
#dev.off()

# Create faceted plot for overlap by trial type
bxp <- ggplot(sm_navTestTrials_path_long, aes(x = DV, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ trialType, scales = "free_y", ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 14))
#jpeg("path_variables_trialType.jpeg", width = 5, height = 5, units = 'in', res = 500)
bxp
#dev.off()



