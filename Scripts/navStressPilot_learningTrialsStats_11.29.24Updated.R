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

# add column with percentages of how much of the path participants recreated (not including grids shared between inner and outer paths)
# City 1 <- outer: 124; inner: 110, novel: 441

# Input data for updated analyses
inputData <- read_excel("manualCombined_pilot_LearningTrialsOverlap.xlsx")

# Make a copy to work with
myData <- inputData

# fix the structure of the data
myData$subjectID <- as.factor(myData$subjectID)
myData$first_route_learned <- as.factor(myData$first_route_learned)
myData$second_route_learned <- as.factor(myData$second_route_learned)
myData$outer_reps <- as.factor(myData$outer_reps)
myData$inner_reps <- as.factor(myData$inner_reps)
myData$more_familiar <- as.factor(myData$more_familiar)
myData$less_familiar <- as.factor(myData$less_familiar)

# add columns for percentages
myData$percent_outer_correct <- myData$overlap_outer_correct/124 #percent of the outer path used when outer path recreated
myData$percent_outer_incorrect <- myData$overlap_outer_incorrect/110 #percent of inner path used when outer path recreated
myData$percent_outer_novel <- myData$novel_grids_outer/441 #percent of novel paths used when outer path recreated
myData$percent_inner_correct <- myData$overlap_inner_correct/110 #percent of inner path used when inner path recreated
myData$percent_inner_incorrect <- myData$overlap_inner_incorrect/124 #percent of outer path used when inner path recreated
myData$percent_inner_novel <- myData$novel_grids_inner/441 #percent of novel paths used when outer path recreated

# columns for adjusted correct percentage for paths
myData$adj_outer_correct <- myData$percent_outer_correct - myData$percent_outer_incorrect - myData$percent_outer_novel
myData$adj_inner_correct <- myData$percent_inner_correct - myData$percent_inner_incorrect - myData$percent_inner_novel

# smaller data frame to work with
smData <- myData %>%
  select(subjectID, more_familiar, adj_outer_correct, adj_inner_correct)

# make the data long
smData_long <- pivot_longer(cols = c(adj_outer_correct, adj_inner_correct), names_to = "route", values_to = "percent", data = smData)

# add column called familiarity labeling routes as more or less familiar
smData_long <- smData_long %>%
  mutate(familiarity = case_when(
    route == "adj_outer_correct" & more_familiar == "outer" ~ "more",
    route == "adj_inner_correct" & more_familiar == "inner" ~ "more",
    TRUE ~ "less"
  ))

hist(smData$adj_outer_correct - smData$adj_inner_correct)

outliers <- smData_long %>%
  identify_outliers(percent)

# Remove the outliers of P002, P016, P018, P022 from the original table
cleaned_smData <- smData_long %>%
  filter(!subjectID %in% c("P002", "P016", "P018", "P022"))

# without outliers
t.test(percent ~ familiarity, paired = TRUE, data = cleaned_smData) # sig p = 0.0448

fam_percent_plot <- ggplot(cleaned_smData, aes(x = familiarity, y = percent)) +
  geom_boxplot(outliers = FALSE) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Familiarity", y = "Adjusted Percent Correct") +
  theme_classic() +
  scale_x_discrete(labels = c("Less", "More")) + 
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_fam_percent.jpeg", width = 4, height = 4, units = 'in', res = 500)
fam_percent_plot
#dev.off()

# with outliers
fam_percent_plot_outliers <- ggplot(smData_long, aes(x = familiarity, y = percent)) +
  geom_boxplot(outliers = FALSE) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Familiarity", y = "Adjusted Percent Correct") +
  theme_classic() +
  scale_x_discrete(labels = c("Less", "More")) + 
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_fam_percent_wOutliers.jpeg", width = 4, height = 4, units = 'in', res = 500)
fam_percent_plot_outliers
#dev.off()

# with outliers
t.test(percent ~ familiarity, paired = TRUE, data = smData_long) # not sig p = .88

# Now make a graph for path learned first

# smaller data frame to work with
pathOrderData <- myData %>%
  select(subjectID, first_route_learned, adj_outer_correct, adj_inner_correct)

# make the data long
pathOrderData_long <- pivot_longer(cols = c(adj_outer_correct, adj_inner_correct), 
                                   names_to = "route", values_to = "percent", data = pathOrderData)

# add column called order labeling routes as first or second
pathOrderData_long <- pathOrderData_long %>%
  mutate(pathOrder = case_when(
    first_route_learned == "outer" & route == "adj_outer_correct" ~ "first",
    first_route_learned == "inner" & route == "adj_inner_correct" ~ "first",
    TRUE ~ "second"
  ))

outliers <- pathOrderData_long %>%
  identify_outliers(percent)

# Remove the outliers of P002, P016, P018, P022 from the original table
cleaned_pathOrderData <- pathOrderData_long %>%
  filter(!subjectID %in% c("P002", "P016", "P018", "P022"))

t.test(percent ~ pathOrder, paired = TRUE, data = cleaned_pathOrderData)

order_percent_plot <- ggplot(cleaned_pathOrderData, aes(x = pathOrder, y = percent)) +
  geom_boxplot(outliers = FALSE) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Path Learning Order", y = "Adjusted Percent Correct") +
  scale_x_discrete(labels = c("First", "Second")) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_order_percent.jpeg", width = 4, height = 4, units = 'in', res = 500)
order_percent_plot
#dev.off()

t.test(percent ~ pathOrder, paired = TRUE, data = cleaned_pathOrderData) # not sig

# with outliers
order_percent_plot_outliers <- ggplot(pathOrderData_long, aes(x = pathOrder, y = percent)) +
  geom_boxplot(outliers = FALSE) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Path Learning Order", y = "Adjusted Percent Correct") +
  scale_x_discrete(labels = c("First", "Second")) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_order_percent_wOutlier.jpeg", width = 4, height = 4, units = 'in', res = 500)
order_percent_plot_outliers
#dev.off()

t.test(percent ~ pathOrder, paired = TRUE, data = pathOrderData_long) # not sig

# Now make this graph for repetitions

# smaller data frame to work with
repsData <- myData %>%
  select(subjectID, outer_reps, inner_reps, adj_outer_correct, adj_inner_correct)

# make the data long
repsData_long <- pivot_longer(cols = c(adj_outer_correct, adj_inner_correct), names_to = "route", values_to = "percent", data = repsData)

# add column called familiarity labeling routes as more or less familiar
repsData_long <- repsData_long %>%
  mutate(repNum = case_when(
    route == "adj_outer_correct" ~ outer_reps,
    route == "adj_inner_correct" ~ inner_reps
  ))

outliers <- repsData_long %>%
  identify_outliers(percent)

# Remove the outliers of P002, P016, P018, P022 from the original table
cleaned_repsData <- repsData_long %>%
  filter(!subjectID %in% c("P002", "P016", "P018", "P022"))

reps_percent_plot <- ggplot(cleaned_repsData, aes(x = repNum, y = percent)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Learning Repetitions", y = "Adjusted Percent Correct") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_reps_percent.jpeg", width = 4, height = 4, units = 'in', res = 500)
reps_percent_plot
#dev.off()

# with outliers
reps_percent_plot_outliers <- ggplot(repsData_long, aes(x = repNum, y = percent)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Learning Repetitions", y = "Adjusted Percent Correct") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_reps_percent_wOutliers.jpeg", width = 4, height = 4, units = 'in', res = 500)
reps_percent_plot_outliers
#dev.off()

# Summary values
summary_rep_values <- repsData_long %>%
  group_by(repNum) %>%
  summarize(
    count = n(),
    adj_percent = mean(percent),
    sd_adj_percent = sd(percent)
  )
summary_rep_values

# without outliers
res.aov <- aov(percent ~ repNum, data = cleaned_repsData)
summary(res.aov) # marginal

# with outliers
res.aov <- aov(percent ~ repNum, data = repsData_long)
summary(res.aov) 

# Now with just outer and inner path
outerInnerData <- myData %>%
  select(subjectID, adj_outer_correct, adj_inner_correct)

# make the data long
outerInnerData_long <- pivot_longer(cols = c(adj_outer_correct, adj_inner_correct), names_to = "route", values_to = "percent", data = outerInnerData)

# add column called familiarity labeling routes as more or less familiar
outerInnerData_long <- outerInnerData_long %>%
  mutate(pathType = case_when(
    route == "adj_outer_correct" ~ "outer",
    route == "adj_inner_correct" ~ "inner"
  ))

# Remove the outliers of P002, P016, P018, P022 from the original table
cleaned_outerInnerData <- outerInnerData_long %>%
  filter(!subjectID %in% c("P002", "P016", "P018", "P022"))

# graph
outerInner_percent_plot <- ggplot(cleaned_outerInnerData, aes(x = pathType, y = percent)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Path Type", y = "Adjusted Percent Correct") +
  scale_x_discrete(labels = c("Inner", "Outer")) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_innerOuter_percent.jpeg", width = 4, height = 4, units = 'in', res = 500)
outerInner_percent_plot
#dev.off()

t.test(percent ~ pathType, paired = TRUE, data = cleaned_outerInnerData) # not sig


# now with outliers
outerInner_percent_plot_outliers <- ggplot(outerInnerData_long, aes(x = pathType, y = percent)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID), color = "gray") +
  labs(x = "Path Type", y = "Adjusted Percent Correct") +
  scale_x_discrete(labels = c("Inner", "Outer")) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#jpeg("D:/Nav Stress Data/dissertation/pics/pilot_innerOuter_percent_wOutliers.jpeg", width = 4, height = 4, units = 'in', res = 500)
outerInner_percent_plot_outliers
#dev.off()

t.test(percent ~ pathType, paired = TRUE, data = outerInnerData_long) # not sig



