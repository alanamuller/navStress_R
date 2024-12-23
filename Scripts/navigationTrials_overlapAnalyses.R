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
library(pbkrtest)
library(BayesFactor)

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
cort_data <- read.csv("mean_cort_table_nmol_L.csv")

demographic_data <- read.xlsx("NavStressDemographics.xlsx") # read in demographics
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

# add a new column for if efficiency matches familiarity (there's only two groups, match or mismatch)
navTrialsData <- navTrialsData %>%
  mutate(effMatchFam = case_when(
    moreFamiliarPath == "outer" ~ "mismatch",
    moreFamiliarPath == "inner" ~ "match"
  ))

# add a column to label subjectID with match values
navTrialsData$subjectID_match <- paste0(navTrialsData$subjectID,navTrialsData$effMatchFam)

# make these new columns factors
navTrialsData$effMatchFam <- as.factor(navTrialsData$effMatchFam)
navTrialsData$subjectID_match <- as.factor(navTrialsData$subjectID_match)

hist(navTrialsData$grid_count)
navTrialsData$log_grid_count <- log(navTrialsData$grid_count)
hist(navTrialsData$log_grid_count)
shapiro.test(navTrialsData$log_grid_count) # still sig but looks better

# add new columns categorizing more familiar grid num and less familiar grid num collapsing across path type
for (i in 1:nrow(navTrialsData)) {
  if (navTrialsData$moreFamiliarPath[i] == "outer") {
    navTrialsData$moreFamGridNum[i] <- navTrialsData$total_outer_use[i]
    navTrialsData$lessFamGridNum[i] <- navTrialsData$total_inner_use[i]
  } else if (navTrialsData$moreFamiliarPath[i] == "inner") {
    navTrialsData$moreFamGridNum[i] <- navTrialsData$total_inner_use[i]
    navTrialsData$lessFamGridNum[i] <- navTrialsData$total_outer_use[i]
  }
}

# add new columns categorizing more familiar grid proportion of path and less familiar proportion type collapsing across path type
for (i in 1:nrow(navTrialsData)) {
  if (navTrialsData$moreFamiliarPath[i] == "outer") {
    navTrialsData$moreFamGridProp[i] <- navTrialsData$outer_proportion[i]
    navTrialsData$lessFamGridProp[i] <- navTrialsData$inner_proportion[i]
  } else if (navTrialsData$moreFamiliarPath[i] == "inner") {
    navTrialsData$moreFamGridProp[i] <- navTrialsData$inner_proportion[i]
    navTrialsData$lessFamGridProp[i] <- navTrialsData$outer_proportion[i]
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
#################### And was it affected by familiarity and cortisol?

# gather total inner and outer use and novel grids
# name the columns you want to gather, the other columns will remain there
#gathered_columns <- c("novel_grids", "total_outer_use", "total_inner_use")
gathered_columns <- c("novel_proportion", "outer_proportion", "inner_proportion")

# gather the data for grid numbers
longNav <- gather(no_lost_navTrials, key = grid_type, value = grid_proportion, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_summary <- longNav %>%
  group_by(subjectID, grid_type, moreFamiliarPath, condition) %>%
  summarize(
    count = n(),
    mean_grid_prop = mean(grid_proportion, na.rm = TRUE), 
    sd_grid_prop = sd(grid_proportion, na.rm = TRUE)
  )

# MANUSCRIPT PIC

tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsMean <- ggplot(nav_summary, aes(x = grid_type, y = mean_grid_prop, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Proportion", fill = "Condition") +
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
#jpeg("D:/Nav Stress Data/dissertation/pics/navTrialsMeanProp.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
navTrialsMean
#dev.off()

# trying an anova with match/mismatch
# one data point per person per grid type
match_summary <- longNav %>%
  group_by(subjectID, grid_type, moreFamiliarPath, condition) %>%
  summarize(
    count = n(),
    mean_grid_prop = mean(grid_proportion, na.rm = TRUE), 
    sd_grid_prop = sd(grid_proportion, na.rm = TRUE)
  )

match_summary <- as.data.frame(match_summary)
match.aov <- anova_test(data = match_summary, dv = mean_grid_prop, wid = subjectID, 
                        within = grid_type, between = c(condition, moreFamiliarPath))
anova_table <- get_anova_table(match.aov) # grid_type, grid_type_moreFamiliarPath sig
#write.csv(anova_table, "D:/Nav Stress Data/dissertation/diss_fam_anova_table.csv", row.names = FALSE)

# Bayes Factor
bayes_rm <- anovaBF(mean_grid_prop ~ condition*grid_type*moreFamiliarPath + subjectID, data = match_summary, whichRandom = "subjectID")
bayes_rm
plot(bayes_rm)

# collapse condition since it wasn't significant
matchNoCond_summary <- longNav %>%
  group_by(subjectID, grid_type, moreFamiliarPath) %>%
  summarize(
    count = n(),
    mean_grid_prop = mean(grid_proportion, na.rm = TRUE), 
    sd_grid_prop = sd(grid_proportion, na.rm = TRUE)
  )

# Effect of grid_type at each familiar
match.way <- matchNoCond_summary %>%
  group_by(moreFamiliarPath) %>%
  anova_test(dv = mean_grid_prop, wid = subjectID, within = grid_type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
match.way

# Pairwise comparisons between grid_type groups
pwc <- matchNoCond_summary %>%
  group_by(moreFamiliarPath) %>%
  pairwise_t_test(
    mean_grid_prop ~ grid_type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc # inners are sig, outers don't survive p adjustment
#write.csv(pwc, "D:/Nav Stress Data/dissertation/diss_fam_pwc_anova_table.csv", row.names = FALSE)

# Effect of familiar at each grid_type
match2.way <- matchNoCond_summary %>%
  group_by(grid_type) %>%
  anova_test(dv = mean_grid_prop, wid = subjectID, within = moreFamiliarPath) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
match2.way # outer and inner sig

# MANUSCRIPT PIC
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

fam_grid_plot <- ggplot(matchNoCond_summary, aes(x = grid_type, y = mean_grid_prop, fill = moreFamiliarPath)) +
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = moreFamiliarPath), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Proportion", fill = "More Familiar") +
  scale_fill_discrete(labels = c("Inner Path", "Outer Path")) +
  theme_classic() +
  scale_x_discrete(labels = tick_labels) + 
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("D:/Nav Stress Data/dissertation/pics/fam_grid_plot.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
fam_grid_plot
#dev.off()


wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")

navTrialsMean <- ggplot(nav_summary, aes(x = grid_type, y = mean_grid_prop, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Proportion", fill = "Condition") +
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
#jpeg("D:/Nav Stress Data/dissertation/pics/navTrialsMeanProp_byFamiliar.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
navTrialsMean
#dev.off()

# table of trials per condition per participant
table_subj_cond <- originalData %>%
  group_by(subjectID, block, condition) %>%
  summarize(
    count = n(),
    mean = mean(grid_count)
  )

# table of trials per condition per participant
subj_table <- originalData %>%
  group_by(subjectID, condition) %>%
  summarize(
    count = n(),
    mean = mean(grid_count)
  )

ggplot(subj_table, aes(x = condition, y = mean)) +
  geom_boxplot()

subj_table <- as.data.frame(subj_table)
subj_table$subjectID <- as.factor(subj_table$subjectID)
subj_table$condition <- as.factor(subj_table$condition)
res.aov <- anova_test(data = subj_table, dv = mean, wid = subjectID, 
                      within = condition)
get_anova_table(res.aov) # no diff - total grid count is the same for all conditions


# rep measures mixed ANOVA

# normality check
normality_each_cond <- nav_summary %>%
  group_by(condition, grid_type) %>%
  shapiro_test(mean_grid_prop)
normality_each_cond # 2 non sig but overall, really not bad

nav_summary %>%
  group_by(grid_type) %>%
  levene_test(mean_grid_prop ~ condition) # good homogeneity of variance

nav_summary <- as.data.frame(nav_summary)
                                
res.aov <- anova_test(data = nav_summary, dv = mean_grid_prop, wid = subjectID, 
                      within = c(condition, grid_type))
get_anova_table(res.aov) # grid type is sig

# Bayes Factor
bayes_rm <- anovaBF(mean_grid_prop ~ condition*grid_type + subjectID, data = nav_summary, whichRandom = "subjectID")
bayes_rm
plot(bayes_rm)

# comparisons for grid_type variable
nav_summary %>%
  pairwise_t_test(
    mean_grid_prop ~ grid_type, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# Try a linear mixed effects model
basic.lm <- lmer(mean_grid_prop ~ grid_type*condition + (1|subjectID), data = nav_summary)
summary(basic.lm)

# Add moreFamiliarPath to the model
addFam.lm <- lmer(mean_grid_prop ~ grid_type*condition*moreFamiliarPath + (1|subjectID), data = nav_summary)
summary(addFam.lm)

anova(basic.lm, addFam.lm) # addFam.lm is the better model according to AIC but not BIC




# graph for variance for iNAV
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsSD <- ggplot(nav_summary, aes(x = grid_type, y = sd_grid_prop, fill = condition)) + 
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

# quick graph with excess path
smData <- longNav %>%
  select(subjectID, block, trial, Navigate_excessPath, condition) %>%
  group_by(subjectID, condition) %>%
  summarize(
    count = n(), 
    meanExcessPath = mean(Navigate_excessPath, na.rm = TRUE)
  )

ggplot(smData, aes(x = condition, y = meanExcessPath)) + 
  geom_boxplot() + geom_jitter()

smData <- as.data.frame(smData)
sm.aov <- anova_test(data = smData, dv = meanExcessPath, wid = subjectID, 
                      within = condition)
get_anova_table(sm.aov) # not sig

########## Data wrangling to get a high and low familiarity category

highLowFam <- nav_summary %>%
  filter(grid_type == "outer_proportion" | grid_type == "inner_proportion")

highLowFam <- highLowFam %>%
  mutate(fam_level = case_when(
    (grid_type == "outer_proportion" & moreFamiliarPath == "outer") ~ "high",
    (grid_type == "inner_proportion" & moreFamiliarPath == "inner") ~ "high",
    (grid_type == "outer_proportion" & moreFamiliarPath == "inner") ~ "low",
    (grid_type == "inner_proportion" & moreFamiliarPath == "outer") ~ "low",
    TRUE ~ NA_character_  # This will handle any unexpected cases
  ))
highLowFam$fam_level <- as.factor(highLowFam$fam_level)

# Graph with familiarity
wrap_labels <- c("Cold Pressor", "Control", "Fire")
names(wrap_labels) <- c("cp", "ctrl", "fire")
tick_labels <- c("Outer Grids", "Inner Grids")

fam_level_plot <- ggplot(highLowFam, aes(x = grid_type, y = mean_grid_prop, fill = fam_level)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = fam_level), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Grid Type", y = "Mean Grid Proportion", fill = "Familiarity") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Familiarity", labels = c("High", "Low"), type = c("#00BA38", "#619CFF")) +
  theme_classic() +
  facet_wrap(vars(condition), labeller = labeller(condition = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
#jpeg("D:/Nav Stress Data/dissertation/pics/condition_fam_prop.jpeg", width = 10, height = 6, units = 'in', res = 500)
fam_level_plot
#dev.off()

# summarize data for linear model
fam_summary <- highLowFam %>%
  group_by(subjectID, grid_type, fam_level, condition) %>%
  summarize(
    count = n(), 
    avg_grid_prop = mean(mean_grid_prop)
  )

fam_summary <- as.data.frame(fam_summary)

test_lm <- lm(avg_grid_prop ~ grid_type*condition*fam_level, data = fam_summary)
summary(test_lm)

# trying fam condition anova
fam_anova <- fam_summary %>%
  group_by(subjectID, fam_level, condition) %>%
  summarize(
      count = n(), 
      bigavg_grid_prop = mean(avg_grid_prop))

fam_anova <- as.data.frame(fam_anova)
res.aov <- anova_test(data = fam_anova, dv = bigavg_grid_prop, wid = subjectID, 
                      within = c(condition, fam_level))
get_anova_table(res.aov) # grid type is sig

ggplot(fam_anova, aes(x = fam_level, y = bigavg_grid_prop, fill = condition)) +
  geom_boxplot() + geom_jitter()

##################### Do people use the same paths forward, backward, and diagonal?

# one data point per person per grid type
trial_type_summary <- longNav %>%
  group_by(subjectID, trial_type, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_prop = mean(grid_proportion, na.rm = TRUE), 
    sd_grid_prop = sd(grid_proportion, na.rm = TRUE)
  )

# MANUSCRIPT PIC
wrap_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")
names(wrap_labels) <- c("novel_proportion", "outer_proportion", "inner_proportion")


trialType <- ggplot(trial_type_summary, aes(x = trial_type, y = mean_grid_prop, fill = condition)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  labs(x = "Trial Type", y = "Mean Grid Proportion") +
  scale_x_discrete(labels = c("Backward", "Diagonal", "Forward")) +
  theme_classic() +
  facet_wrap(vars(grid_type), labeller = labeller(grid_type = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
#jpeg("D:/Nav Stress Data/dissertation/pics/trialType_gridType_cond_prop.jpeg", width = 12, height = 5.75, units = 'in', res = 500)
trialType
#dev.off()

### 3-way repeated ANOVA
trial_type_summary <- as.data.frame(trial_type_summary)

withinTest <- anova_test(data = trial_type_summary, dv = mean_grid_prop, wid = subjectID,
                         within = c(trial_type, grid_type, condition))
grid_type_table <- get_anova_table(withinTest) # grid type, trial type:grid type

#write.csv(grid_type_table, "D:/Nav Stress Data/dissertation/diss_gridTypeTable.csv", row.names = FALSE)

# Bayes Factor
bayes_rm <- anovaBF(mean_grid_prop ~ trial_type*condition*grid_type + subjectID, data = trial_type_summary, whichRandom = "subjectID")
bayes_rm
plot(bayes_rm)

# collapse across condition since there was no effect
collapse_cond <- trial_type_summary %>%
  group_by(subjectID, trial_type, grid_type) %>%
  summarize(
    count = n(), 
    avg_grid_prop = mean(mean_grid_prop)
  )
# post-hoc tests
one.way <- collapse_cond %>%
  group_by(trial_type) %>%
  anova_test(dv = avg_grid_prop, wid = subjectID, within = grid_type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

pwc <- collapse_cond %>%
  group_by(trial_type) %>%
  pairwise_t_test(
    avg_grid_prop ~ grid_type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# now the other way
# post-hoc tests
one2.way <- collapse_cond %>%
  group_by(grid_type) %>%
  anova_test(dv = avg_grid_prop, wid = subjectID, within = trial_type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one2.way

pwc <- collapse_cond %>%
  group_by(grid_type) %>%
  pairwise_t_test(
    avg_grid_prop ~ trial_type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

ggplot(collapse_cond, aes(x = trial_type, y = avg_grid_prop, fill = grid_type)) +
geom_boxplot() + geom_jitter()

##### Now do this again with trial type instead of grid type
highLowFam_trialType <- longNav %>%
  group_by(subjectID, grid_type, trial_type, moreFamiliarPath, condition) %>%
  summarize(
    count = n(),
    mean_grid_prop = mean(grid_proportion, na.rm = TRUE), 
    sd_grid_prop = sd(grid_proportion, na.rm = TRUE)
  )

highLowFam_trialType <- highLowFam_trialType %>%
  filter(grid_type == "outer_proportion" | grid_type == "inner_proportion")

highLowFam_trialType <- highLowFam_trialType %>%
  mutate(fam_level = case_when(
    (grid_type == "outer_proportion" & moreFamiliarPath == "outer") ~ "high",
    (grid_type == "inner_proportion" & moreFamiliarPath == "inner") ~ "high",
    (grid_type == "outer_proportion" & moreFamiliarPath == "inner") ~ "low",
    (grid_type == "inner_proportion" & moreFamiliarPath == "outer") ~ "low",
    TRUE ~ NA_character_  # This will handle any unexpected cases
  ))
highLowFam_trialType$fam_level <- as.factor(highLowFam_trialType$fam_level)

famTrialType_summary <- highLowFam_trialType %>%
  group_by(subjectID, trial_type, grid_type, fam_level, condition) %>%
  summarize(
    count = n(), 
    avg_grid_prop = mean(mean_grid_prop)
  )
famTrialType_summary <- as.data.frame(famTrialType_summary)

# graph
wrap_labels <- c("Cold Pressor", "Control", "Fire")
names(wrap_labels) <- c("cp", "ctrl", "fire")
tick_labels <- c("Backward", "Diagonal", "Forward")

fam_grid_TrialType_plot <- ggplot(famTrialType_summary, aes(x = trial_type, y = avg_grid_prop, fill = fam_level)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = fam_level), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Trial Type", y = "Mean Grid Proportion") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Familiarity", labels = c("High", "Low"), type = c("#00BA38", "#619CFF")) +
  facet_wrap(vars(condition), labeller = labeller(condition = wrap_labels)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
#jpeg("D:/Nav Stress Data/dissertation/pics/fam_grid_prop_trialType.jpeg", width = 12, height = 5.75, units = 'in', res = 500)
fam_grid_TrialType_plot
#dev.off()

# anova
res.aov <- anova_test(data = famTrialType_summary, dv = avg_grid_prop, wid = subjectID, 
                      within = c(condition, grid_type, trial_type))
get_anova_table(res.aov) # grid_type, trial type, and grid type:trial type sig

res.aov <- anova_test(data = famTrialType_summary, dv = avg_grid_prop, wid = subjectID, 
                      within = c(condition, trial_type, fam_level))
get_anova_table(res.aov) # trial type and fam_level sig


# try it again with more familiar path instead of path high low
famGridType_trialType <- longNav %>%
  group_by(subjectID, grid_type, trial_type, moreFamiliarPath, condition) %>%
  summarize(
    count = n(),
    mean_grid_prop = mean(grid_proportion, na.rm = TRUE), 
    sd_grid_prop = sd(grid_proportion, na.rm = TRUE)
  )

famGridType_trialType <- as.data.frame(famGridType_trialType)
#res.aov <- anova_test(data = famGridType_trialType, dv = mean_grid_prop, wid = subjectID, 
 #                     within = c(grid_type, trial_type, moreFamiliarPath))
get_anova_table(res.aov) 

##### redo the graph above with good and bad navigators
bad_navs <- nav_summary %>%
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
good_navs <- nav_summary %>%
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
normality <- nav_summary %>%
  group_by(condition, grid_type, moreFamiliarPath) %>%
  shapiro_test(mean_grid_number) # 6 are sig not normal but rest are ok

# ARTool analysis - need some help here
n <- art(mean_grid_number ~ condition*grid_type*moreFamiliarPath + Error(condition), data = nav_summary)
summary(n) # still may not be appropriate
anova(n) # lots of sig

# try regular anova - same as ARTool mostly
res.aov <- aov(mean_grid_number ~ condition*grid_type*moreFamiliarPath, data = nav_summary)
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

nav_summary2 <- subset(nav_summary2, mean_grid_number < upperGridRange)

# graph for iNAV
tick_labels <- c("Novel Grids", "More Familiar Path Grids", "Less Familiar Path Grids")

navPlot <- ggplot(nav_summary2, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
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

# create data frame
optimal_lost_subj <- navTrialsData %>%
  group_by(subjectID) %>%
  summarize(
    optimal_nav_count = sum(excess_block_num <= 2), # allow for a little variation when finding stores
    lost_trials_count = sum(grid_count > mean_gridCount+(3*sd_gridCount))
  )

sum(optimal_lost_subj$optimal_nav_count) # 1155
sum(optimal_lost_subj$lost_trials_count) # 63

hist(optimal_lost_subj$optimal_nav_count)
shapiro.test(optimal_lost_subj$optimal_nav_count) # it's normal

optimal_lost_subj_SOT <- merge(optimal_lost_subj, sotData, by = "subjectID") 

optimal_by_condition <- navTrialsData %>%
  group_by(subjectID, condition) %>%
  summarize(
    optimal_nav_count = sum(excess_block_num <= 2), # allow for a little variation when finding stores
    lost_trials_count = sum(grid_count > mean_gridCount+(2.5*sd_gridCount))
  )

# Plot for number of optimal trials by condition
optimal_trials <- ggplot(optimal_by_condition, aes(x = condition, y = optimal_nav_count)) + 
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point(color = "gray80") +
  geom_line(aes(group = subjectID), color = "gray80") +
  stat_summary(aes(group = condition), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Condition", y = "Mean Number of Optimal Trials") +
  scale_x_discrete(labels = c("Cold Pressor", "Control","Fire")) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
  
#jpeg("D:/Nav Stress Data/dissertation/pics/optimal_trials.jpeg", width = 7, height = 6, units = 'in', res = 500)
optimal_trials
#dev.off()

# ANOVA
optimal_by_condition <- as.data.frame(optimal_by_condition)

opt.res.aov <- anova_test(data = optimal_by_condition, dv = optimal_nav_count, wid = subjectID, 
                      within = condition)
get_anova_table(opt.res.aov) # not sig

# check angular error for outliers - 1 at 116 SOT error
mean_SOT <- mean(optimal_lost_subj_SOT$SOT_average_angular_error)
sd_SOT <- sd(optimal_lost_subj_SOT$SOT_average_angular_error)

optimal_lost_subj_SOT <- subset(optimal_lost_subj_SOT, SOT_average_angular_error < mean_SOT + (3*sd_SOT))

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

#jpeg("E:/Nav Stress Data/dissertation/pics/sotLostCorr_SD.jpeg", width = 4, height = 5, units = 'in', res = 500)
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



##### Did participants' excess path correlate with their 



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

##### Learning by block

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
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/excessPathPlot.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
excessPathPlot
#dev.off()

# Stats for learning by block - COME BACK TO THIS ANALYSIS, IT'S NOT THAT IMPORTANT
excessPathSummary <- as.data.frame(excessPathSummary)

learn_blocks <- anova_test(data = excessPathSummary, dv = mean_excess_path, wid = subjectID,
                         within = c(block, condition))
get_anova_table(learn_blocks) # only block sig

# post-hoc 
pwc <- excessPathSummary %>%
  group_by(condition) %>%
  pairwise_t_test(
    mean_excess_path ~ block, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


avgExcessPathSubj <- longNav2 %>%
  group_by(subjectID, condition) %>%
  summarise(
    count = n(),
    mean_excessPath = mean(Navigate_excessPath),
    sd_excessPath = sd(Navigate_excessPath)
  )

avgExcessPathSubj <- avgExcessPathSubj %>%
  rename("subjNum" = 1)

# Write bigMergeData to a new csv file
#write.csv(avgExcessPathSubj, "E:/Nav Stress Data/avgExcessPathSubj.csv", row.names = FALSE)


##### Correlations with cortisol and measures
# make one big data sheet
bigData <- merge(auc_data_long, excess_path_data, by = c("subjNum", "condition"))
bigData$log_excessPath <- log(bigData$mean_excessPath)

# Fixing the structure of the dataframe
bigData$subjNum <- as.factor(bigData$subjNum)
bigData$condition <- as.factor(bigData$condition)
bigData$gender <- as.factor(bigData$gender)


# log_auc and log excess path
plot(bigData$auc_log, bigData$log_excessPath)
cor.test(bigData$auc_log, bigData$log_excessPath) # not sig

aucLog_excessPath_condition <- ggplot(bigData, aes(x = auc_log, y = log_excessPath, color = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, aes(fill = condition), alpha = 0.08) +
  theme_classic() +
  labs(x = "AUC (Log)", y = "Excess Path (Log)",
       color = "Condition", fill = "Condition") +
  scale_color_manual(name = "Condition", 
                     values = c("ctrl" = "green", "cp" = "blue", "fire" = "red"),  # Change colors as needed
                     labels = c("ctrl" = "Control", "cp" = "Cold Pressor", "fire" = "Fire")) +  # Custom labels
  scale_fill_manual(name = "Condition", 
                    values = c("ctrl" = "green", "cp" = "blue", "fire" = "red"),  # Ensure fill colors match
                    labels = c("ctrl" = "Control", "cp" = "Cold Pressor", "fire" = "Fire")) +  # Custom labels
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 13))

#jpeg("D:/Nav Stress Data/dissertation/pics/auc_logExcessPath.jpeg", width = 7, height = 6, units = 'in', res = 500)
aucLog_excessPath_condition
#dev.off()

# The baseline corrected one looks weird because the fire condition has less variance than the other two conditions
aucBC_excessPath_condition <- ggplot(bigData, aes(x = auc_bc, y = log_excessPath, color = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "AUC Baseline Corrected", y = "Excess Path (Log)") +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))

aucBC_excessPath_condition


# AUC and excess path
plot(bigData$auc_bc, bigData$log_excessPath)
cor.test(bigData$auc_bc, bigData$log_excessPath) # not sig

# AUC and excess path by condition
auc_cp <- bigData %>%
  filter(condition == "cp")
auc_ctrl <- bigData %>%
  filter(condition == "ctrl")
auc_fire <- bigData %>%
  filter(condition == "fire")

plot(auc_cp$auc_log, auc_cp$log_excessPath)
cor.test(auc_cp$auc_log, auc_cp$log_excessPath) # not sig for cp

plot(auc_ctrl$auc_log, auc_ctrl$log_excessPath)
cor.test(auc_ctrl$auc_log, auc_ctrl$log_excessPath) # not sig for ctrl

plot(auc_fire$auc_log, auc_fire$log_excessPath)
cor.test(auc_fire$auc_log, auc_fire$log_excessPath) # not sig for fire


##### surveys

# make a dataframe combining all the surveys











