library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(magrittr)
library(dplyr)

# Start fresh by removing everything from the environment
rm(list = ls())

# Set working directory
setwd("E:/Nav Stress Data/Salimetrics reports") # for hard drive

# Enter the data 
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

samples9Data$cort_nmol_L <- samples9Data$mean_cort*276
samples12Data$cort_nmol_L <- samples12Data$mean_cort*276

all_data <- rbind(samples9Data, samples12Data)

small_data <- samples9Data %>% 
  filter(subjNum >= 2 & subjNum <= 3)

data_ctrl <- samples9Data %>%
  filter(condition == "ctrl")

data_cp <- samples9Data %>%
  filter(condition == "cp")

data_fire <- samples9Data %>%
  filter(condition == "fire")

summaryStats <- samples9Data%>%
  group_by(condition, time) %>%
  get_summary_stats(mean_cort, type = "mean_sd")

# Set the order of the x-axis
level_order <- c('pre', 'post1', 'post15', 'post30')


##### Pics of all data

# Plot with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

ggplot(data = all_data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

bxp <- ggboxplot(
  all_data, x = "time", y = "cort_nmol_L", 
  color = "condition"
)
bxp

##### Pics of 9 sample data

# Plot with all participant separated by condition and time
ggplot(data = samples9Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

ggplot(data = samples9Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

##### Pics of 12 sample data

# Plot with all participant separated by condition and time
ggplot(data = samples12Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

ggplot(data = samples12Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

########## Checking assumptions

##### All data

# all_data outliers - 4 outliers for now
outliers_allData <- all_data %>%
  group_by(time, condition) %>%
  identify_outliers(cort_nmol_L)

mean_cort <- mean(all_data$cort_nmol_L)
sd_cort <- sd(all_data$cort_nmol_L)

no_outliers_allData <- subset(all_data, all_data$cort_nmol_L > mean_cort - sd_cort*2.5 & all_data$cort_nmol_L < mean_cort + sd_cort*2.5)

no_outliers_allData$log_cort <- log(no_outliers_allData$cort_nmol_L)

# Checking normality
ggqqplot(no_outliers_allData$cort_nmol_L)
hist(no_outliers_allData$cort_nmol_L)
hist(log(no_outliers_allData$cort_nmol_L))

normality_allData <- no_outliers_allData %>%
  group_by(time) %>%
  shapiro_test(log_cort)

# quick and dirty anova
res.aov <- anova_test(data = no_outliers_allData, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov)

# testing main effects since the interaction wasn't sig

# Make a plot
cond.labs <- c("Cold Pressor", "Control Condition", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
ggplot(data = no_outliers_allData, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol (log nmol/L)" )

