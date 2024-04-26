# Nav Stress Prelim Behavioral Analyses

library(openxlsx)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstatix)

rm(list = ls())

##### Read in file with subject, condition, and city data
setwd("E:/Nav Stress Data/Participant_data/") # set working directory

myData <- read.xlsx("navTrialsLog.xlsx") # read in file
sotData <- read.xlsx("SOT_data.xlsx") # read in SOT data


# Make some columns factors
myData$subjectID <- as.factor(myData$subjectID)
myData$block <- as.factor(myData$block)
myData$trial <- as.factor(myData$trial)
myData$condition <- as.factor(myData$condition)
myData$city <- as.factor(myData$city)
myData$trial_type <- as.factor(myData$trial_type)

########## Histograms
hist(myData$Navigate_excessPath)
hist(myData$Navigate_duration)

### Get rid of outliers in excess path and duration

mean_excess <- mean(myData$Navigate_excessPath)
sd_excess <- sd(myData$Navigate_excessPath)

mean_duration <- mean(myData$Navigate_duration)
sd_duration <- sd(myData$Navigate_duration)

cleanData <- subset(myData, myData$Navigate_excessPath > mean_excess - sd_excess*2.5 & myData$Navigate_excessPath < mean_excess + sd_excess*2.5)
cleanData <- subset(cleanData, cleanData$Navigate_duration > mean_duration - sd_duration*2.5 & cleanData$Navigate_duration < mean_duration + sd_duration*2.5)

hist(cleanData$Navigate_excessPath)
hist(cleanData$Navigate_duration)

### Data has a positive skew - log transform
# There are negative numbers in the Navigate_excessPath column, find them
neg_nums <- cleanData$Navigate_excessPath[cleanData$Navigate_excessPath < 0]
# Delete the negative nums
cleanData <- subset(cleanData, cleanData$Navigate_excessPath > 0)
# Log transform
cleanData$log_excessPath <- log(cleanData$Navigate_excessPath)
hist(cleanData$log_excessPath) # so normal now wow
ggqqplot(cleanData$log_excessPath)

# Log transform duration
cleanData$log_duration <- log(cleanData$Navigate_duration)
hist(cleanData$log_duration) # better but still not great
ggqqplot(cleanData$log_duration)

ggboxplot(cleanData, x = "subjectID", y = "log_excessPath", facet.by = "trial")

ggqqplot(log(myData$Navigate_excessPath))

# Collapse the data over trials
blockCondition <- cleanData %>%
  group_by(subjectID, condition, block) %>%
  summarize(mean_log_excess = mean(log_excessPath, na.rm = TRUE), 
            sd_log_excess = sd(log_excessPath, na.rm = TRUE),
            mean_log_dur = mean(log_duration, na.rm = TRUE), 
            sd_log_dur = sd(log_duration, na.rm = TRUE))

blockCondition <- as.data.frame(blockCondition) # make into a dataframe because anova doesn't like tibble

blockTrialType <- cleanData %>%
  group_by(subjectID, trial_type, block) %>%
  summarize(mean_log_excess = mean(log_excessPath, na.rm = TRUE), 
            sd_log_excess = sd(log_excessPath, na.rm = TRUE),
            mean_log_dur = mean(log_duration, na.rm = TRUE), 
            sd_log_dur = sd(log_duration, na.rm = TRUE))

blockTrialType <- as.data.frame(blockTrialType) # make into a dataframe because anova doesn't like tibble

########## Some quick anova's
# anova - log excess path by block and condition
res.aov_excess <- anova_test(data = blockCondition, dv = mean_log_excess, wid = subjectID, within = c(condition, block))
get_anova_table(res.aov_excess)

# anova - log duration by block and condition
res.aov_dur <- anova_test(data = blockCondition, dv = mean_log_dur, wid = subjectID, within = c(condition, block))
get_anova_table(res.aov_dur)

# anova - log excess path by block and trial type
res.aov_excess_tt <- anova_test(data = blockTrialType, dv = mean_log_excess, wid = subjectID, within = c(trial_type, block))
get_anova_table(res.aov_excess_tt)

# anova - log duration by block and trial type
res.aov_dur_tt <- anova_test(data = blockTrialType, dv = mean_log_dur, wid = subjectID, within = c(trial_type, block))
get_anova_table(res.aov_dur_tt)

# anova - trial type

##### box plots to visualize data
# Log excess path
bxp_excess <- ggboxplot(
  cleanData, x = "condition", y = "log_excessPath", 
  color = "block", add = "jitter")
bxp_excess

# Log duration
bxp_dur <- ggboxplot(
  cleanData, x = "condition", y = "log_duration", 
  color = "block", add = "jitter")
bxp_dur

# Trial type
bxp_trialType_excess <- ggboxplot(
  cleanData, x = "trial_type", y = "log_excessPath", 
  color = "block", add = "jitter")
bxp_trialType_excess

bxp_trialType_dur <- ggboxplot(
  cleanData, x = "trial_type", y = "log_duration", 
  color = "block", add = "jitter")
bxp_trialType_dur

########## SOT corr with excess path and duration
small_data <- cleanData %>%
  group_by(subjectID) %>%
  summarize(mean_log_excess = mean(log_excessPath, na.rm = TRUE), 
            sd_log_excess = sd(log_excessPath, na.rm = TRUE),
            mean_log_dur = mean(log_duration, na.rm = TRUE), 
            sd_log_dur = sd(log_duration, na.rm = TRUE))
  
# combine the data from SOT to the small data frame to make sure data from the same subjects are paired
small_data$SOT_average <- sotData$SOT_average
small_data$SOT_stdev <- sotData$SOT_stdev

# get rid of outliers
outliers_small_data <- small_data %>%
  identify_outliers(SOT_average)

mean_SOT <- mean(small_data$SOT_average)
sd_SOT <- sd(small_data$SOT_average)

NO_small_data <- subset(small_data, small_data$SOT_average > mean_SOT - sd_SOT*2.5 & small_data$SOT_average < mean_SOT + sd_SOT*2.5)

# plot SOT by mean log excess path
x <- NO_small_data$SOT_average
y <- NO_small_data$mean_log_excess

ggplot(NO_small_data, aes(x = x, y = y)) +
  geom_point() +
  theme_classic() +
  labs(x = "Average SOT Score", y = "Mean Log Excess Path") +
  stat_cor(method = "pearson", label.x = 30, label.y = 6) +
  stat_smooth(method = "lm",
              formula = y ~ x, 
              geom = "smooth")

# plot SOT by sd log excess path
x <- NO_small_data$SOT_average
y <- NO_small_data$sd_log_excess

ggplot(NO_small_data, aes(x = x, y = y)) +
  geom_point() +
  theme_classic() +
  labs(x = "Average SOT Score", y = "SD Log Excess Path") +
  stat_cor(method = "pearson", label.x = 30, label.y = 1.5) +
  stat_smooth(method = "lm",
              formula = y ~ x, 
              geom = "smooth")

# plot SOT by mean log duration
x <- NO_small_data$SOT_average
y <- NO_small_data$mean_log_dur

ggplot(NO_small_data, aes(x = x, y = y)) +
  geom_point() +
  theme_classic() +
  labs(x = "Average SOT Score", y = "Mean Log Duration") +
  stat_cor(method = "pearson", label.x = 30, label.y = 4) +
  stat_smooth(method = "lm",
              formula = y ~ x, 
              geom = "smooth")

# plot SOT by sd log duration
x <- NO_small_data$SOT_average
y <- NO_small_data$sd_log_dur

ggplot(NO_small_data, aes(x = x, y = y)) +
  geom_point() +
  theme_classic() +
  labs(x = "Average SOT Score", y = "SD Log Duration") +
  stat_cor(method = "pearson", label.x = 30, label.y = 1) +
  stat_smooth(method = "lm",
              formula = y ~ x, 
              geom = "smooth")
