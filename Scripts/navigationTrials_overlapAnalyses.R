# Overlap Stats

##### Navigation Trials Overlap Stats
# Did people previously learned routes or new ones to get to their destination?

# Try ARTools for nonparametric factorial ANOVA

library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ARTool)
library(openxlsx)
library(dplyr)
library(lme4)

rm(list = ls())

##### Read in data
setwd("E:/Nav Stress Data/") # set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress shortcuts") # for developing

################################# look at nav trials data

originalData <- read.csv("combined_navTrialsLogData.csv") # keep a copy of original data
navTrialsData <- originalData # work with this copy of the data

sotData <- read.xlsx("SOT_data.xlsx") # read in the SOT data
sbsod_stress_surveys <- read.csv("sbsod_perceivedStress_scored.csv") # read in the SBSOD scores and perceived stress scores

good_bad_nav_labels <- read.csv("good_bad_nav_labels.csv") # read in the bad, good, and great navigator labels
auc_data <- read.csv("auc_data.csv") # read in completed cases for area under the curve cortisol data
auc_data_long <- read.csv("auc_long_data.csv") # read in long version of auc table

demographic_data <- read_excel("NavStressDemographics.xlsx") # read in demographics
excess_path_data <- read.csv("avgExcessPathSubj.csv") # read in excess path for each participant

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

# add columns for proportion of path that was inner, outer, and novel
# use the grid count without the overlap
navTrialsData$outer_proportion <- navTrialsData$total_outer_use/navTrialsData$grid_count
navTrialsData$inner_proportion <- navTrialsData$total_inner_use/navTrialsData$grid_count
navTrialsData$novel_proportion <- navTrialsData$novel_grids/navTrialsData$grid_count

# add new column for log grid_count
navTrialsData$log_grid_count <- log(navTrialsData$grid_count)

# add new columns categorizing more familiar grid type and less familiar grid type collapsing across path type
for (i in 1:nrow(navTrialsData)) {
  if (navTrialsData$moreFamiliarPath[i] == "outer") {
    navTrialsData$moreFamGridNum[i] <- navTrialsData$total_outer_use[i]
    navTrialsData$lessFamGridNum[i] <- navTrialsData$total_inner_use[i]
  } else if (navTrialsData$moreFamiliarPath[i] == "inner") {
    navTrialsData$moreFamGridNum[i] <- navTrialsData$total_inner_use[i]
    navTrialsData$lessFamGridNum[i] <- navTrialsData$total_outer_use[i]
  }
}

# Exclude the freebie trials from city 3 (when the start and target store were the same) by excluding participants who traveled less than 300 units on a trial (10 trials)
navTrialsData <- subset(navTrialsData, Navigate_actualPath > 300)
# exclude trials with too many negative excess block numbers - coding error to be fixed soon on city 3 trial 13
navTrialsData <- subset(navTrialsData, excess_block_num > -3)
# exclude trials where Ss got hopelessly lost
# find outliers in grid count and we'll call them lost
mean_gridCount <- mean(navTrialsData$grid_count)
sd_gridCount <- sd(navTrialsData$grid_count)

# create dataset with no lost trials
no_lost_navTrials <- subset(navTrialsData, grid_count < mean_gridCount+(2.5*sd_gridCount))

hist(no_lost_navTrials$grid_count) # still skewed but better
hist(log(no_lost_navTrials$grid_count))
shapiro.test(log(no_lost_navTrials$grid_count)) # still skewed, doesn't pass shapiro test

# create dataset with only lost trials
lost_trials <- which(navTrialsData$grid_count > mean_gridCount+(2.5*sd_gridCount))
length(lost_trials) # 87 lost trials

lost_Ss <- subset(navTrialsData, grid_count > mean_gridCount+(2.5*sd_gridCount))
hist(lost_Ss$grid_count)

#################### What percentage of new/old/novel paths were taken on no lost trials?

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
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

# check for outliers
meanGrid <- mean(nav_summary$mean_grid_number)
SDGrid <- sd(nav_summary$sd_grid_number)

upperGridRange <- meanGrid + (2.5*SDGrid)

NO_nav_summary <- subset(nav_summary, mean_grid_number < upperGridRange) # 19 outliers

# graph for iNAV

tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsMean <- ggplot(NO_nav_summary, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition") +
  theme_classic() +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("E:/Nav Stress Data/dissertation/pics/navTrialsMean.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
navTrialsMean
#dev.off()

wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")

navTrialsMean <- ggplot(NO_nav_summary, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition") +
  theme_classic() +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
#jpeg("E:/Nav Stress Data/dissertation/pics/navTrialsMean_byFamiliar.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
navTrialsMean
#dev.off()


# rep measures mixed ANOVA - needs fixing to account for moreFamiliarPath

# normality check
normality_each_cond <- NO_nav_summary %>%
  group_by(condition, grid_type) %>%
  shapiro_test(mean_grid_number)
normality_each_cond # 3 non sig but overall, really not bad

NO_nav_summary %>%
  group_by(grid_type) %>%
  levene_test(mean_grid_number ~ condition) # good homogeneity of variance

NO_nav_summary <- as.data.frame(NO_nav_summary)
                                
res.aov <- anova_test(data = NO_nav_summary, dv = mean_grid_number, wid = subjectID, 
                      within = c(condition, grid_type))
get_anova_table(res.aov) # grid type is sig

# Try a linear mixed effects model
basic.lm <- lm(mean_grid_number ~ condition*grid_type, data = NO_nav_summary)
summary(basic.lm)



# graph for variance for iNAV
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsSD <- ggplot(NO_nav_summary, aes(x = grid_type, y = sd_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "SD", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("E:/Nav Stress Data/dissertation/pics/navTrialsIQR.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
navTrialsSD
#dev.off()

##### redo the graph above with good and bad navigators
bad_navs <- NO_nav_summary %>%
  filter(subjectID == 15 | subjectID == 16 | subjectID == 22 | subjectID == 23 | subjectID == 30)

wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

badNavs <- ggplot(bad_navs, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("E:/Nav Stress Data/dissertation/pics/navTrialsMedian.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
badNavs
#dev.off()

# Participants to exclude
participants_to_exclude <- c(15, 16, 22, 23, 30)

# Exclude data from the specified participants
good_navs <- NO_nav_summary %>%
  filter(!subjectID %in% participants_to_exclude)

# redo the graph with only good and great navigators
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

goodNavs <- ggplot(good_navs, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("E:/Nav Stress Data/dissertation/pics/navTrialsMedian.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
goodNavs
#dev.off()


# check normality
normality <- NO_nav_summary %>%
  group_by(condition, grid_type, moreFamiliarPath) %>%
  shapiro_test(mean_grid_number) # 6 are sig not normal but rest are ok

# ARTool analysis - need some help here
n <- art(mean_grid_number ~ condition*grid_type*moreFamiliarPath + Error(condition), data = NO_nav_summary)
summary(n) # still may not be appropriate
anova(n) # lots of sig

# try regular anova - same as ARTool mostly
res.aov <- aov(mean_grid_number ~ condition*grid_type*moreFamiliarPath, data = NO_nav_summary)
summary(res.aov)

# one data point per person per grid type
nav_summary2 <- longNav %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

ggplot(nav_summary2, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge())

# graph that connects each person's data points
nav_summary2 <- longNav %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

ggplot(nav_summary2, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  facet_wrap(vars(condition)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID))

##### do that again but with more and less familiar grid numbers

# gather more fam and less fam grid nums and novel grids
# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "moreFamGridNum", "lessFamGridNum")

# gather the data for grid numbers
longNav2 <- gather(no_lost_navTrials, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# check normality
longNav2 %>%
  group_by(condition, grid_type) %>%
  shapiro_test(grid_number) # everything is not normal - try log transformation

longNav2$log_grid_number <- log(longNav2$grid_number + 2) # +2 because some values have -1 and 0

# recheck normality
longNav2 %>%
  group_by(condition, grid_type) %>%
  shapiro_test(log_grid_number) # still super not normal so lets do the ARTool package

# ARTool analysis
m <- art(grid_number ~ condition*grid_type + Error(city), data = longNav2)
summary(m) # says ART may not be appropriate
anova(m)

nav_summary2 <- longNav2 %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

# check for outliers
meanGrid <- mean(nav_summary2$mean_grid_number)
sdGrid <- sd(nav_summary2$sd_grid_number)

upperGridRange <- meanGrid + (2.5*sdGrid)

NO_nav_summary2 <- subset(nav_summary2, mean_grid_number < upperGridRange)

# graph for iNAV
tick_labels <- c("Novel Grids", "More Familiar Path Grids", "Less Familiar Path Grids")

navPlot <- ggplot(NO_nav_summary2, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("E:/Nav Stress Data/dissertation/pics/navMoreLessFam.jpeg", width = 9.5, height = 5.75, units = 'in', res = 500)
navPlot
#dev.off()
  

#################### How often did participants have the optimal path?
hist(no_lost_navTrials$excess_block_num)

optimal_trials <- which(no_lost_navTrials$excess_block_num <= 1)
length(optimal_trials) # 879 optimal trials

optimal_trials <- subset(no_lost_navTrials, excess_block_num <= 1)

# gather the data for grid numbers
optimal_nav <- gather(optimal_trials, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_summary <- optimal_nav %>%
  group_by(subjectID, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

# graph for Kailee

ggplot(nav_summary, aes(x = condition, y = mean_grid_number)) + 
  geom_boxplot(outliers = FALSE) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition")

# code a column for number of optimal paths for each Ss

optimal_lost_subj <- navTrialsData %>%
  group_by(subjectID) %>%
  summarize(
    optimal_nav_count = sum(excess_block_num <= 2), # allow for a little variation when finding stores
    lost_trials_count = sum(grid_count > mean_gridCount+(2.5*sd_gridCount))
  )

sum(optimal_lost_subj$optimal_nav_count)
sum(optimal_lost_subj$lost_trials_count)

hist(optimal_lost_subj$optimal_nav_count)
shapiro.test(optimal_lost_subj$optimal_nav_count) # it's normal

optimal_lost_subj_SOT <- merge(optimal_lost_subj, sotData, by = "subjectID") 

optimal_by_condition <- navTrialsData %>%
  group_by(subjectID, condition) %>%
  summarize(
    optimal_nav_count = sum(excess_block_num <= 2), # allow for a little variation when finding stores
    lost_trials_count = sum(grid_count > mean_gridCount+(2.5*sd_gridCount))
  )

ggplot(optimal_by_condition, aes(x = condition, y = optimal_nav_count)) + 
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID))




# check angular error for outliers - 1 at 116 SOT error
mean_SOT <- mean(optimal_lost_subj_SOT$SOT_average_angular_error)
sd_SOT <- sd(optimal_lost_subj_SOT$SOT_average_angular_error)

optimal_lost_subj_SOT <- subset(optimal_lost_subj_SOT, SOT_average_angular_error < mean_SOT + (2.5*sd_SOT))

plot(optimal_lost_subj_SOT$optimal_nav_count, optimal_lost_subj_SOT$SOT_average_angular_error)
cor.test(optimal_lost_subj_SOT$optimal_nav_count, optimal_lost_subj_SOT$SOT_average_angular_error) # sig, p = .0288

plot(optimal_lost_subj_SOT$lost_trials_count, optimal_lost_subj_SOT$SOT_average_angular_error)
cor.test(optimal_lost_subj_SOT$lost_trials_count, optimal_lost_subj_SOT$SOT_average_angular_error) # dumb but sig

# maybe graph for INAV
sp <- ggscatter(optimal_lost_subj_SOT, x = "lost_trials_count", y = "SOT_average_angular_error", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Number of Lost Trials",
                ylab = "SOT Avg Angular Error") + 
  stat_cor(method = "pearson", label.x = 7, label.y = 15)

#jpeg("E:/Nav Stress Data/dissertation/pics/sotLostCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
sp
#dev.off()

# with sd now
sp <- ggscatter(optimal_lost_subj_SOT, x = "lost_trials_count", y = "SOT_stdev", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Number of Lost Trials",
                ylab = "SOT SD") + 
  stat_cor(method = "pearson", label.x = 7, label.y = 15)

#jpeg("E:/Nav Stress Data/dissertation/pics/sotLostCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
sp
#dev.off()

sp <- ggscatter(sotData, x = "SOT_average_angular_error", y = "SOT_stdev", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Average Angular Error",
                ylab = "SOT SD") + 
  stat_cor(method = "pearson", label.x = 60, label.y = 15)

#jpeg("E:/Nav Stress Data/dissertation/pics/sotLostCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
sp
#dev.off()

#################### How often did participants get lost?

# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "total_outer_use", "total_inner_use")

# gather the data for grid numbers
long_lost <- gather(lost_Ss, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_lost <- long_lost %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

ggplot(nav_lost, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  geom_jitter(position = position_jitterdodge())

# Do people use the same paths forward, backward, and diagonal?

# one data point per person per grid type
trial_type_summary <- longNav %>%
  group_by(subjectID, trial_type, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE)
  )

# check for outliers
meanGrid <- mean(trial_type_summary$mean_grid_number)
sdGrid <- IQR(trial_type_summary$sd_grid_number)

upperGridRange <- meanGrid + (2.5*sdGrid)

trial_type_summary <- subset(trial_type_summary, mean_grid_number < upperGridRange)









wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")

navTrialsMean <- ggplot(NO_nav_summary, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Number", fill = "Condition") +
  theme_classic() +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels))












wrap_labels <- c("Novel Grids Used", "Outer Grids Used", "Inner Grids Used")
names(wrap_labels) <- c("novel_grids", "total_outer_use", "total_inner_use")

# graph for iNAV
trialType <- ggplot(trial_type_summary, aes(x = trial_type, y = mean_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  labs(x = "Trial Type", y = "Mean Grid Number") +
  theme_classic() +
  facet_wrap(vars(grid_type), labeller = labeller(grid_type = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  scale_x_discrete(labels = c("Backward", "Diagonal", "Forward"))
jpeg("E:/Nav Stress Data/dissertation/pics/trialType.jpeg", width = 12, height = 5.75, units = 'in', res = 500)
trialType
dev.off()

# stats for this figure
nav_lm <- lmer(mean_grid_number ~ condition + (1 | subjectID), data = trial_type_summary)
summary(nav_lm)

# 2-way repeated ANOVA
withinTest <- anova_test(data = trial_type_summary, dv = mean_grid_number, wid = subjectID,
                         within = c(backward, diagonal, forward))
get_anova_table(withinTest) # everything is sig

# post-hoc tests
one.way <- aov_data %>%
  group_by(path_recreated_cat) %>%
  anova_test(dv = mean_percent, wid = subjectID, within = percent_cat) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# have to take out 22 and 26 or pairwise won't work
aov_data_22_26_gone <- aov_data %>%
  filter(!(subjectID %in% c(22,26)))

pwc <- aov_data_22_26_gone %>%
  group_by(percent_cat) %>%
  pairwise_t_test(
    mean_percent ~ path_recreated_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc




######### excess path analysis

# Excess path analyses

# Did participants have more excess path for condition and path familiarity?
excessPathSummary <- longNav2 %>%
  group_by(subjectID, block, condition) %>%
  summarize(
    count = n(),
    mean_excess_path = mean(Navigate_excessPath),
    sd_excess_path = sd(Navigate_excessPath)
  )

excessPathPlot <- ggplot(excessPathSummary, aes(x = condition, y = mean_excess_path, fill = block)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge(), color = "gray40") +
  labs(x = "Condition", y = "Mean Excess Path", fill = "Block") +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13)) +
  scale_x_discrete(labels = c("Cold Pressor", "Control", "Fire Environment"))
jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/excessPathPlot.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
excessPathPlot
dev.off()

avgExcessPathSubj <- longNav2 %>%
  group_by(subjectID, condition) %>%
  summarise(
    count = n(),
    mean_excessPath = mean(Navigate_excessPath),
    sd_excessPath = sd(Navigate_excessPath)
  )

# Write bigMergeData to a new csv file
#write.csv(avgExcessPathSubj, "E:/Nav Stress Data/avgExcessPathSubj.csv", row.names = FALSE)


##### Correlations with cortisol and excess path
# make one big data sheet
test_bigData <- merge(auc_data, excess_path_data, by.x = 1, by.y = 1, all.x = TRUE, all.y = TRUE)


# baseline cort and excess path
plot(excess_path_data$mean_excessPath, auc_data$ctrl)

# AUC and excess path


# AUC bc and excess path













