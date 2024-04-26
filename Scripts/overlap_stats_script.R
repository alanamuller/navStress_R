# Overlap Stats

##### Recreate Paths Overlap Stats
# Did people learn the routes initially and did it stick around to the end?

# Try ARTools for nonparametric factorial ANOVA

library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

rm(list = ls())

##### Read in data
setwd("E:/Nav Stress Data/") # set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress shortcuts") # for developing

myData <- read.csv("combined_recreatePathsLogData.csv") # read in file

# add column with percentages of how much of the path participants recreated (not including grids shared between inner and outer paths)
# City 1 <- outer: 124; inner: 110
# City 2 <- outer: 127; inner: 107
# City 3 <- outer: 136; inner: 111

for (i in 1:nrow(myData)){
  if (myData$city[i] == "city1"){
    myData$percent_grid_overlap_outer[i] <- myData$grid_overlap_outer[i]/124
    myData$percent_grid_overlap_inner[i] <- myData$grid_overlap_inner[i]/110
  } else if (myData$city[i] == "city2"){
    myData$percent_grid_overlap_outer[i] <- myData$grid_overlap_outer[i]/127
    myData$percent_grid_overlap_inner[i] <- myData$grid_overlap_inner[i]/107
  } else if (myData$city[i] == "city3"){
    myData$percent_grid_overlap_outer[i] <- myData$grid_overlap_outer[i]/136
    myData$percent_grid_overlap_inner[i] <- myData$grid_overlap_inner[i]/111
  }
}



# add path type column

# make certain columns a factor
myData$trialname <- as.factor(myData$trialname)
myData$subjectID <- as.factor(myData$subjectID)
myData$condition <- as.factor(myData$condition)
myData$moreFamiliarPath <- as.factor(myData$moreFamiliarPath)
myData$city <- as.factor(myData$city)
myData$path_recreated <- as.factor(myData$path_recreated)

outer_df <- subset(myData, path_recreated == "outer")
inner_df <- subset(myData, path_recreated == "inner")

# this recategorizes the data so I can combine the inner and outer paths into one
# to see if Ss can accurately recreate their path
outer_df$correct_recreation_percent <- outer_df$percent_grid_overlap_outer
outer_df$incorrect_recreation_percent <-outer_df$percent_grid_overlap_inner

inner_df$correct_recreation_percent <- inner_df$percent_grid_overlap_inner
inner_df$incorrect_recreation_percent <-inner_df$percent_grid_overlap_outer

myData <- rbind(outer_df, inner_df)

ggplot(myData, aes(x = myData$subjectID, y = correct_recreation_percent)) +
  geom_boxplot()

ggplot(myData, aes(x = myData$subjectID, y = incorrect_recreation_percent)) +
  geom_boxplot()

moreFamOut <- subset(myData, moreFamiliarPath == "outer")
moreFamIn <- subset(myData, moreFamiliarPath == "inner")

hist(myData$grid_count)
hist(outer_df$grid_overlap_outer)
hist(inner_df$grid_overlap_inner)
hist(myData$novel_grids)

### going from wide to long data
# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("grid_overlap_outer", "grid_overlap_inner", "novel_grids")

# gather the data for grid numbers
longGrid <- gather(myData, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# Graph with raw numbers of grids - one data point per grid_type per person
grid_type_table <- longGrid %>% 
  group_by(subjectID, grid_type, path_recreated, moreFamiliarPath) %>%
  summarize(
    mean_gridNum = mean(grid_number),
    sd_gridNum = sd(grid_number),
    median_gridNum = median(grid_number), 
    IQR_gridNum = IQR(grid_number)
  )

# This graph for Kailee
wrap_labels <- c("Inner Path Recreated", "Outer Path Recreated")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Outer Path", "Inner Path", "Novel Grids")

ggplot(grid_type_table, aes(x = grid_type, y = median_gridNum, fill = moreFamiliarPath)) +
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  facet_wrap(vars(path_recreated), labeller = labeller(path_recreated = wrap_labels)) +
  labs(x = "Grid Type Overlap", y = "Median Grid Number", fill = "More Familiar Path") +
  scale_x_discrete(labels = tick_labels) + scale_fill_discrete(name = "More Familiar Path", labels = c("Inner", "Outer"))
  

####################

bxp <- ggboxplot(
  grid_type_table, x = "grid_type", y = "median_gridNum", fill = "moreFamiliarPath", bxp.errorbar = TRUE, 
  add = "jitter", facet.by = "path_recreated")
bxp

# name the columns you want to gather, the other columns will remain there
percent_gathered_columns <- c("percent_grid_overlap_outer", "percent_grid_overlap_inner")

# gather the data for grid percentages
longPercentGrid <- gather(myData, key = grid_type, value = grid_percent, percent_gathered_columns, factor_key = TRUE)

# gather the correct and incorrect trials to plot them and do stats
long_corr_incorr <- gather(myData, key = trial_type, value = recreation_percent, c(correct_recreation_percent, incorrect_recreation_percent))

hist(longPercentGrid$grid_percent)
hist(myData$correct_recreation_percent)
hist(myData$incorrect_recreation_percent)

means <- longPercentGrid %>% 
  group_by(path_recreated, grid_type) %>%
  summarize(
    mean_gridPercent = mean(grid_percent),
    sd_gridPercent = sd(grid_percent)
  )

bxp <- ggboxplot(
  longPercentGrid, x = "path_recreated", y = "grid_percent", color = "grid_type"
)

bxp
# identify outliers
longPercentGrid %>%
  group_by(path_recreated, grid_type) %>%
  identify_outliers(grid_percent)
# get rid of outlier
longPercentGrid_NO <- longPercentGrid %>%
  filter(!subjectID == 2)

bxp <- ggboxplot(
  longPercentGrid_NO, x = "path_recreated", y = "grid_percent", color = "grid_type"
)
bxp

# normality - nothing is normal at all
longPercentGrid_NO %>%
  group_by(path_recreated, grid_type) %>%
  shapiro_test(grid_percent)

ggqqplot(longPercentGrid_NO, "grid_percent", ggtheme = theme_bw()) +
  facet_grid(path_recreated ~ grid_type, labeller = "label_both")

res.aov <- anova_test(
  data = longPercentGrid_NO, dv = grid_percent, wid = subjectID,
  within = c(longPercentGrid_NO$grid_type, path_recreated)
)
get_anova_table(res.aov)

# when Ss recreated the outer/inner path, they have more overlap with that path - good sanity check
ggplot(longGrid, aes(x = path_recreated, y = grid_number, fill = grid_type)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(vars(city))

# now a boxplot with the percentages instead
ggplot(longPercentGrid, aes(x = path_recreated, y = grid_percent, fill = grid_type)) +
  geom_boxplot() + 
  facet_wrap(vars(city))

summary_stats <- longGrid %>%
  group_by(grid_type, moreFamiliarPath) %>%
  get_summary_stats(grid_number, type = "common")


ggplot(longGrid, aes(x = moreFamiliarPath, y = grid_number, fill = grid_type)) + 
  geom_boxplot(outliers = FALSE)

hist(longGrid$grid_number)


### Kruskal wallis (one way anova nonparametric) for more/less familiar path recreations

# there are an unequal number of observations in each cell
xtabs(~ moreFamiliarPath + path_recreated, data = myData)

famOut_recreatedOut <- outer_df %>%
  filter(moreFamiliarPath == "outer")
famIn_recreatedOut <- outer_df %>%
  filter(moreFamiliarPath == "inner")
famOut_recreatedIn <- inner_df %>%
  filter(moreFamiliarPath == "outer")
famIn_recreatedIn <- inner_df %>%
  filter(moreFamiliarPath == "inner")

moreLessFam_outer <- outer_df %>%
  group_by(subjectID, moreFamiliarPath) %>%
  summarize(
    count = n(),
    mean_percent_overlap_outer = mean(percent_grid_overlap_outer, na.rm = TRUE), 
    sd_percent_overlap_outer = sd(percent_grid_overlap_outer, na.rm = TRUE),
    median_percent_overlap_outer = median(percent_grid_overlap_outer, na.rm = TRUE),
    IQR_percent_overlap_outer = IQR(percent_grid_overlap_outer, na.rm = TRUE)
  )

ggplot(moreLessFam_outer, aes(x = moreFamiliarPath, y = median_percent_overlap_outer)) + 
  geom_boxplot() +
  geom_jitter(width = .1)

# test to see if the medians are different: p = .035, they are different
# the more familiar path was learned better for outer
kruskal.test(median_percent_overlap_outer ~ moreFamiliarPath, data = moreLessFam_outer)

# now for the inner path
moreLessFam_inner <- inner_df %>%
  group_by(subjectID, moreFamiliarPath) %>%
  summarize(
    count = n(),
    mean_percent_overlap_inner = mean(percent_grid_overlap_inner, na.rm = TRUE), 
    sd_percent_overlap_inner = sd(percent_grid_overlap_inner, na.rm = TRUE),
    median_percent_overlap_inner = median(percent_grid_overlap_inner, na.rm = TRUE),
    IQR_percent_overlap_inner = IQR(percent_grid_overlap_inner, na.rm = TRUE)
  )

ggplot(moreLessFam_inner, aes(x = moreFamiliarPath, y = median_percent_overlap_inner)) + 
  geom_boxplot() + 
  geom_jitter(width = .1)

# now the test: p = 0.285, the medians are not different could indicate 
kruskal.test(median_percent_overlap_inner ~ moreFamiliarPath, data = moreLessFam_inner)

# now testing recreated paths outer
moreLessFam_outer <- moreFamOut %>%
  group_by(subjectID, path_recreated) %>%
  summarize(
    count = n(),
    mean_percent_overlap_outer = mean(percent_grid_overlap_outer, na.rm = TRUE), 
    sd_percent_overlap_outer = sd(percent_grid_overlap_outer, na.rm = TRUE),
    median_percent_overlap_outer = median(percent_grid_overlap_outer, na.rm = TRUE),
    IQR_percent_overlap_outer = IQR(percent_grid_overlap_outer, na.rm = TRUE)
  )

ggplot(moreLessFam_outer, aes(x = path_recreated, y = median_percent_overlap_outer)) + 
  geom_boxplot() +
  geom_jitter(width = .1)

# test to see if the medians are different: p < .05, they are different
# the more familiar path was learned better for outer
kruskal.test(median_percent_overlap_outer ~ path_recreated, data = moreLessFam_outer)

# now testing recreated paths inner
moreLessFam_inner <- moreFamIn %>%
  group_by(subjectID, path_recreated) %>%
  summarize(
    count = n(),
    mean_percent_overlap_inner = mean(percent_grid_overlap_inner, na.rm = TRUE), 
    sd_percent_overlap_inner = sd(percent_grid_overlap_inner, na.rm = TRUE),
    median_percent_overlap_inner = median(percent_grid_overlap_inner, na.rm = TRUE),
    IQR_percent_overlap_inner = IQR(percent_grid_overlap_inner, na.rm = TRUE)
  )

ggplot(moreLessFam_inner, aes(x = path_recreated, y = median_percent_overlap_inner)) + 
  geom_boxplot() +
  geom_jitter(width = .1)

# test to see if the medians are different: p < .05, they are different
# the more familiar path was learned better for outer
kruskal.test(median_percent_overlap_inner ~ path_recreated, data = moreLessFam_inner)

# Did they learn the route they were supposed to? Is correct different than incorrect
corr_incorr_summary <- long_corr_incorr %>%
  group_by(subjectID, path_recreated, trial_type, moreFamiliarPath) %>%
  summarize(
    count = n(),
    mean_recreation_percent = mean(recreation_percent, na.rm = TRUE), 
    sd_recreation_percent = sd(recreation_percent, na.rm = TRUE),
    median_recreation_percent = median(recreation_percent, na.rm = TRUE),
    IQR_recreation_percent = IQR(recreation_percent, na.rm = TRUE)
  )

# Means table - send to Kailee
corr_incorr_medians_table <- corr_incorr_summary %>%
  group_by(trial_type, path_recreated, moreFamiliarPath) %>%
  summarize(
    count = n(),
    median_recreation_percent = median(median_recreation_percent, na.rm = TRUE),
    MAD_recreation_percent = mad(median_recreation_percent, na.rm = TRUE)
  )

# Send to Kailee
wrap_labels <- c("Inner Path Recreated", "Outer Path Recreated")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Overlap with Recreated Path", "Overlap with Non-Recreated Path")

ggplot(corr_incorr_summary, aes(x = trial_type, y = median_recreation_percent, fill = moreFamiliarPath)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter() +
  labs(x = "Trial Type", y = "Median Recreation Percent", fill = "More Familiar Path") +
  scale_x_discrete(labels = tick_labels) + scale_fill_discrete(name = "More Familiar Path", labels = c("Inner", "Outer")) +
  facet_wrap(vars(path_recreated), labeller = labeller(path_recreated = wrap_labels))

# the test - very sig, different medians
kruskal.test(median_recreation_percent ~ trial_type, data = corr_incorr_summary)

hist(corr_incorr_summary$median_recreation_percent)
################################# look at nav trials data

navTrialsData <- read.csv("combined_navTrialsLogData.csv")

# make the correct variables factors
navTrialsData$subjectID <- as.factor(navTrialsData$subjectID)
navTrialsData$block <- as.factor(navTrialsData$block)
navTrialsData$trial <- as.factor(navTrialsData$trial)
navTrialsData$target_store_num <- as.factor(navTrialsData$target_store_num)
navTrialsData$startEnd_store <- as.factor(navTrialsData$startEnd_store)
navTrialsData$condition <- as.factor(navTrialsData$condition)
navTrialsData$moreFamiliarPath <- as.factor(navTrialsData$moreFamiliarPath)
navTrialsData$city <- as.factor(navTrialsData$city)
navTrialsData$trial_type <- as.factor(navTrialsData$trial_type)

# add new columns totaling outer and inner use
navTrialsData$total_outer_use <- navTrialsData$grid_overlap_outer_seg1 + navTrialsData$grid_overlap_outer_seg2 + navTrialsData$grid_overlap_outer_seg3 + navTrialsData$grid_overlap_outer_seg4
navTrialsData$total_inner_use <- navTrialsData$grid_overlap_inner_seg1 + navTrialsData$grid_overlap_inner_seg2 + navTrialsData$grid_overlap_inner_seg3 + navTrialsData$grid_overlap_inner_seg4

# Exclude the freebie trials from city 3 (when the start and target store were the same) by excluding participants who traveled less than 300 units on a trial
navTrialsData <- subset(navTrialsData, Navigate_actualPath > 300)
# exclude incorrect trial participant 7, city 3 trial 13
navTrialsData <- subset(navTrialsData, excess_block_num > -5)

# How often did participants get lost?

hist(navTrialsData$grid_count) # so very skewed to the right

# find outliers and we'll call them lost
mean_gridCount <- mean(navTrialsData$grid_count)
sd_gridCount <- sd(navTrialsData$grid_count)

# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "total_outer_use", "total_inner_use")

lost_trials <- which(navTrialsData$grid_count > mean_gridCount+(2.5*sd_gridCount))
length(lost_trials) # 27 lost trials

lost_Ss <- subset(navTrialsData, grid_count > mean_gridCount+(2.5*sd_gridCount))
hist(lost_Ss$grid_count)

# gather the data for grid numbers
long_lost <- gather(lost_Ss, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_lost <- long_lost %>%
  group_by(subjectID, grid_type) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

ggplot(nav_lost, aes(x = grid_type, y = median_grid_number)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(width = .1)

# exclude lost trials from the dataset

no_lost_navTrials <- subset(navTrialsData, grid_count < mean_gridCount+(2.5*sd_gridCount))

hist(no_lost_navTrials$excess_block_num) # still skewed but better
hist(log(no_lost_navTrials$excess_block_num)) #
shapiro.test(no_lost_navTrials$excess_block_num)

# gather total inner and outer use and novel grids
# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "total_outer_use", "total_inner_use")

# gather the data for grid numbers
longNav <- gather(no_lost_navTrials, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_summary <- longNav %>%
  group_by(subjectID, grid_type, moreFamiliarPath, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

# graph for Kailee
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Total Outer Grids", "Total Inner Grids")

ggplot(nav_summary, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  labs(x = "Grid Type", y = "Median Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels))


# one data point per person per grid type
nav_summary2 <- longNav %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

ggplot(nav_summary2, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge())

# How often did participants have the optimal path
wrong_excess <- which(no_lost_navTrials$excess_block_num < 0)
length(wrong_excess)


optimal_trials <- which(no_lost_navTrials$excess_block_num <= 0 )
length(optimal_trials) # 201 optimal trials

one_extra <- which(no_lost_navTrials$excess_block_num == 1)
length(one_extra) # 229 just about optimal

hist(no_lost_navTrials$excess_block_num)

# Type of trial graphs

# one data point per person per grid type
trial_type_summary <- longNav %>%
  group_by(subjectID, trial_type, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

ggplot(trial_type_summary, aes(x = trial_type, y = median_grid_number, fill = grid_type)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(width = .1) +
  facet_wrap(vars(condition))











