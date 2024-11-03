library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(ez)
library(ggpubr)
library(rstatix)
library(patchwork)

rm(list = ls())

# Set the directory where the Excel files are stored
setwd("E:/Nav Stress Data")

import_data <- read_excel("pilot_navTestTrials.xlsx", sheet = "Sheet 1")

# Copy of data to play with
df <- import_data

# Fix structure of the data: columns 1:11 should be factors
df <- df %>%
  mutate_at(vars(1:12), as.factor)

### Delete the trials that have a duration less than 1 because those are mistakes
# Count trials to be deleted
count <- sum(df$Navigate_duration < 1)

# Delete those rows
cd <- subset(df, Navigate_duration > 1) # cd stands for "clean data"

### Make some histograms - see the spread of the data
hist(df$Navigate_actualPath)
hist(df$Navigate_optimalPath)
hist(df$Navigate_excessPath)
hist(df$Navigate_duration)
hist(df$overlap_outer)
hist(df$overlap_inner)
hist(df$total_grids_trial)

# Make column for overlap percentages
cd$overlap_outer_percent <- (cd$overlap_outer/cd$total_grids)*100
cd$overlap_inner_percent <- (cd$overlap_inner/cd$total_grids)*100
cd$nonoverlap_percent <- (cd$nonoverlap/cd$total_grids)*100

# Make column for overlap outer-inner
cd$outerMinusInner <- (cd$overlap_outer_percent - cd$overlap_inner_percent)

# Get rid of excess path outliers
navExcess_mean <- mean(cd$Navigate_excessPath, na.rm = TRUE)
navExcess_sd <- sd(cd$Navigate_excessPath, na.rm = TRUE)

count <- sum(cd$Navigate_excessPath < (navExcess_mean - (3*navExcess_sd))) # none
count <- sum(cd$Navigate_excessPath > (navExcess_mean + (3*navExcess_sd))) # 9

cd <- subset(cd, !Navigate_excessPath > (navExcess_mean + (3*navExcess_sd)))


# Get rid of actual path outliers
actualPath_mean <- mean(cd$Navigate_actualPath, na.rm = TRUE)
actualPath_sd <- sd(cd$Navigate_actualPath, na.rm = TRUE)

count <- sum(cd$Navigate_actualPath < (actualPath_mean - (3*actualPath_sd))) # none
count <- sum(cd$Navigate_actualPath > (actualPath_mean + (3*actualPath_sd))) # 9

actualPath_cd <- subset(cd, !Navigate_actualPath > (actualPath_mean + (3*actualPath_sd)))



### Make some graphs

# uncomment this to save manuscript-quality pics to this folder
#setwd("E:/Nav Stress Data/pics/SFN2023")

# Navigation duration by block
nav_dur <- cd %>%
  group_by(subjectID, block, path_type) %>%
  summarise(
    mean_nav_dur = mean(Navigate_duration, na.rm = TRUE),
    sd = sd(Navigate_duration, na.rm = TRUE)
  )

### Plots by subject to get a look at how each did

# excess path
ggplot(cd, aes(x = subjectID, y = Navigate_excessPath)) +
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# excess path by route first learned

# split by which path taught first
plot_left <- ggplot(subset(cd, first_route_learned == "Outer"), aes(x = subjectID, y = Navigate_excessPath)) +
  geom_boxplot() + labs(title = "Outer Route Learned First", x = "Subject Number", y = "Excess Path") + 
  theme_classic() + guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 18), legend.text = element_text(size = 15))

plot_right <- ggplot(subset(cd, first_route_learned == "Inner"), aes(x = subjectID, y = Navigate_excessPath)) +
  geom_boxplot() + labs(title = "Inner Route Learned First", x = "Subject Number", y = "Excess Path") + 
  theme_classic() + guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 18), legend.text = element_text(size = 15))

excessPath_subj <- grid.arrange(plot_left, plot_right, ncol=2)

#ggsave("excessPath_subj.jpeg", excessPath_subj ,width = 12, height = 6, units = 'in', dpi = 500)

# nav duration
ggplot(cd, aes(x = subjectID, y = Navigate_duration)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# split by which path taught first
plot_left <- ggplot(subset(cd, first_route_learned == "Outer"), aes(x = subjectID, y = Navigate_duration)) +
  geom_boxplot() + labs(title = "Outer Route Learned First", x = "Subject Number", y = "Duration") + 
  theme_classic() + guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 18), legend.text = element_text(size = 15))

plot_right <- ggplot(subset(cd, first_route_learned == "Inner"), aes(x = subjectID, y = Navigate_duration)) +
  geom_boxplot() + labs(title = "Inner Route Learned First", x = "Subject Number", y = "Duration") + 
  theme_classic() + guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 18), legend.text = element_text(size = 15))

duration_subj <- grid.arrange(plot_left, plot_right, ncol=2)

#ggsave("duration_subj.jpeg", duration_subj ,width = 12, height = 6, units = 'in', dpi = 500)

# navigation excess path and duration correlation
cor(cd$Navigate_duration, cd$Navigate_excessPath)
plot(cd$Navigate_duration, cd$Navigate_excessPath)

# overlap outer path
ggplot(cd, aes(x = subjectID, y = overlap_outer)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# overlap inner path
ggplot(cd, aes(x = subjectID, y = overlap_inner)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  xlab("Subject Number") +
  ylab("Mean Nav Duration (s)") + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

##### Overlap dv analyses

# outer and inner path on same plot
overlap_df <- cd[c("subjectID", "overlap_outer_percent", "overlap_inner_percent", "nonoverlap_percent", "path_type", "first_route_learned", "outer_reps", "Navigate_excessPath")]
overlap_melt <- melt(overlap_df, id.vars =  c("subjectID", "path_type", "first_route_learned", "outer_reps", "Navigate_excessPath"), 
                     variable.name = "inner_outer_non", value.name = "overlap_percent")

ggplot(overlap_melt, aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() +  theme_classic()

# split by which path taught first
plot_left <- ggplot(subset(overlap_melt, first_route_learned == "Outer"), aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + 
  labs(title = "Outer Route Learned First", x = "Subject Number", y = "Overlap Percent", fill = "Overlap Type") + 
  theme_classic() +
  scale_fill_discrete(labels = c("Outer Overlap", "Inner Overlap", "Novel Percent")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

plot_right <- ggplot(subset(overlap_melt, first_route_learned == "Inner"), aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + 
  labs(title = "Inner Route Learned First", x = "Subject Number", y = "Overlap Percent", fill = "Overlap Type") + 
  theme_classic() +
  scale_fill_discrete(labels = c("Outer Overlap", "Inner Overlap", "Novel Percent")) +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

# Combine plots with a shared legend at the top
overlap_subj <- plot_left + plot_right + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right")

#jpeg("E:/Nav Stress Data/dissertation/pics/pilot_overlap_subj.jpeg", width = 14, height = 6, units = 'in', res = 500)
overlap_subj
#dev.off()

collapse_subj <- overlap_melt %>%
  group_by(subjectID, inner_outer_non, first_route_learned) %>%
  summarize(
    count = n(),
    mean_overlap_percent = mean(overlap_percent)
  )

# now a plot that averages over subject number
avg_plot_left <- ggplot(subset(collapse_subj, first_route_learned == "Outer"), 
                        aes(x = inner_outer_non, y = mean_overlap_percent, fill = inner_outer_non)) +
  geom_boxplot(outliers = FALSE) + 
  geom_jitter() + guides(fill = FALSE) +
  labs(title = "Outer Route Learned First", x = "Overlap Type", y = "Overlap Percent") + 
  scale_x_discrete(labels = c("Outer", "Inner", "Novel")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

avg_plot_right <- ggplot(subset(collapse_subj, first_route_learned == "Inner"), 
                         aes(x = inner_outer_non, y = mean_overlap_percent, fill = inner_outer_non)) +
  geom_boxplot(outliers = FALSE) + 
  geom_jitter() + guides(fill = FALSE) +
  labs(title = "Inner Route Learned First", x = "Overlap Type", y = "Overlap Percent") + 
  scale_x_discrete(labels = c("Outer", "Inner", "Novel")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

# Combine plots with a shared legend at the top
avg_overlap_type <- avg_plot_left + avg_plot_right + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right")

#jpeg("E:/Nav Stress Data/dissertation/pics/pilot_avg_overlap_type.jpeg", width = 8, height = 6, units = 'in', res = 500)
avg_overlap_type
#dev.off()

# run the 2x3 anova (route learned first: inner, outer) (overlap type: outer, inner, novel)
collapse_subj <- as.data.frame(collapse_subj)

res.aov <- anova_test(data = collapse_subj, dv = mean_overlap_percent, wid = subjectID, 
                      within = inner_outer_non, between = first_route_learned)
get_anova_table(res.aov) # everything is sig

mod <- aov(mean_overlap_percent ~ inner_outer_non * first_route_learned,
           data = collapse_subj
)

# print results
summary(mod)

# Uncomment below if you need to change the labels in the legend 
#scale_fill_discrete(labels = c("Outer Percent", "Inner Percent", "Nonoverlap Percent"))



# split by which path taught first and how many reps it got
plot11 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "2"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 2, inner rep 4")
plot12 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "2"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 2, inner rep 4")
plot21 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "4"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 4, inner rep 2")
plot22 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "4"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 4, inner rep 2")
plot31 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "3"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 3, inner rep 6")
plot32 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "3"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 3, inner rep 6")
plot41 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "6"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 6, inner rep 3")
plot42 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "6"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 6, inner rep 3")
grid.arrange(plot11, plot12, plot21, plot22, plot31, plot32, plot41, plot42, ncol=2, nrow=4)

### Plots by path type

path_type_firstLearned <- overlap_melt %>%
  group_by(subjectID, inner_outer_non, path_type, first_route_learned) %>%
  summarize(
    count = n(),
    mean_overlap_percent = mean(overlap_percent)
  )

# plain path type all together
ggplot(path_type_firstLearned, aes(x = path_type, y = mean_overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  scale_fill_brewer(palette = "Paired")

# split by which path was taught first
plot_left <- ggplot(subset(path_type_firstLearned, first_route_learned == "Outer"), 
                    aes(x = path_type, y = mean_overlap_percent, fill = inner_outer_non)) +
  geom_boxplot(outliers = FALSE) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 1, color = "black") +
  labs(title = "Outer Route Learned First", x = "Path Type", y = "Overlap Percent") + 
  scale_x_discrete(labels = c("Backward","Diagonal","Forward")) +
  theme_classic() + guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

plot_right <- ggplot(subset(path_type_firstLearned, first_route_learned == "Inner"), 
                     aes(x = path_type, y = mean_overlap_percent, fill = inner_outer_non)) +
  geom_boxplot(outliers = FALSE) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 1, color = "black") +
  labs(title = "Inner Route Learned First", x = "Path Type", y = "Overlap Percent") + 
  scale_x_discrete(labels = c("Backward","Diagonal","Forward")) +
  theme_classic() + guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 17),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

# Combine plots with a shared legend at the top
overlap_pathType_pathLearned <- plot_left + plot_right + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right")

#jpeg("E:/Nav Stress Data/dissertation/pics/pilot_overlap_pathType_pathLearned.jpeg", width = 9, height = 6, units = 'in', res = 500)
overlap_pathType_pathLearned
#dev.off()

# anova for path learned first, path type, and overlap type
# 3 way anova
path_type_firstLearned <- as.data.frame(path_type_firstLearned)

res.aov <- anova_test(data = path_type_firstLearned, dv = mean_overlap_percent, wid = subjectID, 
                      within = c(inner_outer_non, path_type), between = first_route_learned)
get_anova_table(res.aov) # everything is sig


#ggsave("overlap_pathType.jpeg", overlap_pathType ,width = 9, height = 6, units = 'in', dpi = 500)

##### Navigate excess path dv analyses

navExcess_mean <- mean(cd$Navigate_excessPath, na.rm = TRUE)
navExcess_sd <- sd(cd$Navigate_excessPath, na.rm = TRUE)

count <- sum(cd$Navigate_excessPath < (navExcess_mean - (3*navExcess_sd))) # none
count <- sum(cd$Navigate_excessPath > (navExcess_mean + (3*navExcess_sd))) # 9

cd <- subset(cd, !Navigate_excessPath > (navExcess_mean + (3*navExcess_sd)))

# split by which path taught first and how many reps it got
plot11 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "2"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 2, inner rep 4")
plot12 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "2"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 2, inner rep 4")
plot21 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "4"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 4, inner rep 2")
plot22 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "4"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 4, inner rep 2")
plot31 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "3"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 3, inner rep 6")
plot32 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "3"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 3, inner rep 6")
plot41 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "6"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 6, inner rep 3")
plot42 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "6"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 6, inner rep 3")
grid.arrange(plot11, plot12, plot21, plot22, plot31, plot32, plot41, plot42, ncol=2, nrow=4)

# Outer and inner excess path

# Stats for a t-test
t.test_df <- cd[c("subjectID", "first_route_learned", "rep_condition", "Navigate_excessPath")]
t.test_df <- t.test_df %>%
  group_by(subjectID, first_route_learned, rep_condition) %>%
  summarise(
    mean_nav_excessPath = mean(Navigate_excessPath)
  )
wide_df <- pivot_wider(t.test_df, id_cols = subjectID, names_from = first_route_learned, values_from = mean_nav_excessPath)
wide_df <- wide_df[, -1]

t.test(wide_df$Outer, wide_df$Inner) # not sig

navExcess <- ggplot(t.test_df, aes(x = first_route_learned, y = mean_nav_excessPath, fill = rep_condition)) +
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(position = position_jitterdodge()) +
  labs(x = "First Route Learned", y = "Excess Path (mean)", fill = "Repetition Condition") +
  scale_fill_discrete(labels = c("2/4", "3/6")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 18), legend.text = element_text(size = 15),
        legend.position = "top")

#jpeg("E:/Nav Stress Data/dissertation/pics/excessPath_routeReps.jpeg", width = 6, height = 6, units = 'in', res = 500)
navExcess
#dev.off()
#ggsave("navExcess.jpeg", navExcess ,width = 6, height = 6, units = 'in', dpi = 500)

# ANOVA for rep and excess path conditions
t.test_df <- as.data.frame(t.test_df)

res.aov <- anova_test(data = t.test_df, dv = mean_nav_excessPath, wid = subjectID, 
                      between = c(first_route_learned, rep_condition))
get_anova_table(res.aov) 

### Actual path travelled

ap_df <- actualPath_cd[c("subjectID", "first_route_learned", "rep_condition", "Navigate_actualPath")]
ap_df <- ap_df %>%
  group_by(subjectID, first_route_learned, rep_condition) %>%
  summarise(
    mean_nav_actualPath = mean(Navigate_actualPath)
  )

navExcess <- ggplot(ap_df, aes(x = first_route_learned, y = mean_nav_actualPath, fill = rep_condition)) +
  geom_boxplot() + 
  labs(x = "First Route Learned", y = "Actual Path (mean)", fill = "Repetition Condition") +
  scale_fill_discrete(labels = c("2/4", "3/6")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 18), legend.text = element_text(size = 15),
        legend.position = "top")

ggsave("navExcess.jpeg", navExcess ,width = 6, height = 6, units = 'in', dpi = 500)


##### Stats

### 2 way anova for nav excess path

# create dataframe for stats
excessPath_df <- cd[c("subjectID", "first_route_learned", "rep_condition","Navigate_excessPath")]
ep_df <- excessPath_df %>%
  group_by(subjectID, first_route_learned, rep_condition) %>%
  summarise(
    mean = mean(Navigate_excessPath, na.rm = TRUE)
  )

aov <- aov(mean ~ first_route_learned * rep_condition, data = ep_df)
summary(aov)

ggboxplot(ep_df, x = "first_route_learned", y = "mean", color = "rep_condition")

# anova for inner, outer, and nonoverlap
overlap_df <- cd[c("subjectID", "overlap_outer_percent", "overlap_inner_percent", "nonoverlap_percent")]
overlap_melt <- melt(overlap_df, id.vars = "subjectID", variable.name = "route_type", value.name = "overlap_percent")

overlap_aov <- overlap_melt %>%
  group_by(subjectID, route_type) %>%
  summarise(
    mean_overlap_percent = mean(overlap_percent)
  )

ggboxplot(overlap_aov, x = "route_type", y = "mean_overlap_percent")

# Perform paired one-way ANOVA
result <- aov(mean_overlap_percent ~ route_type + Error(subjectID/route_type), data = overlap_aov)
summary(result)

pwc <- overlap_melt %>%
  pairwise_t_test(
    overlap_percent ~ route_type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
