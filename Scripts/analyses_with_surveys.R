# Correlations with surveys

### Data Collection Surveys
# 1. Demographics
# 2. Santa Barbara Sense of Direction 
# 3. Scene and Orientation-Dependent Pointing Task (SOT)
# 4. Perceived Stress survey

library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(Hmisc)
library(psych)
library(correlation)
library(corrplot)
library(Hmisc)

rm(list = ls())

##### Read in data
# setwd("E:/Nav Stress Data/surveys/") # set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress shortcuts") # for developing
setwd("D:/UA_stuff/seagate_hard_drive/Nav Stress Data")

demog_data <- readxl::read_excel("NavStressDemographics.xlsx", sheet = "NavStressRData")
sbsod_perceivedStress_data <- read.csv("sbsod_perceivedStress_scored.csv") # read in survey file

sot_data <- read.xlsx("SOT_data.xlsx")
avgExcessPathSubj <- read.csv("avgExcessPathSubj.csv") # read in excess path file
learningTrials <- read.csv("learningTrialsBigMergeData.csv") # read in the learning file)
nav_data <- read.csv("nav_grid_proportion_byID.csv")
auc_data <- read.csv("auc_data.csv")


##### First change participant ID so be the same for all the datasets

# First change participant ID to subjectID
demog_data <- demog_data %>%
  rename(subjectID = `Participant ID`) %>%
  mutate(subjectID = as.numeric(subjectID))
sbsod_perceivedStress_data <- sbsod_perceivedStress_data %>%
  rename(subjectID = Participant.ID) %>%
  mutate(subjectID = as.numeric(subjectID))

##### Do basic means for the demographics and surveys
# For demographics: sex, age, video game experience, race

# First rename the demographic variables
demog_data <- demog_data %>%
  rename(
    Sex = `Sex assigned at birth`,
    video_game_exp = `How would you rate you exposure-to/experience-with video games?`
  )

# Count sex
count_sex <- demog_data %>%
  count(Sex) # Female = 17, Male = 14

# Age mean, sd, and range
age_stats <- demog_data %>%
  summarise(
    Mean = mean(Age, na.rm = TRUE), # 21.64516
    SD = sd(Age, na.rm = TRUE), # 3.665591
    Min = min(Age, na.rm = TRUE), # 18
    Max = max(Age, na.rm = TRUE) # 32
  )

# Video game experience mean, sd, and range
vge_stats <- demog_data %>%
  summarise(
    Mean = mean(video_game_exp, na.rm = TRUE), # 3.419355
    SD = sd(video_game_exp, na.rm = TRUE), # 1.088552
    Min = min(video_game_exp, na.rm = TRUE), # 1
    Max = max(video_game_exp, na.rm = TRUE) # 5
  )

# Race
count_race <- demog_data %>%
  count(Race)

### Now do the means, sd, and ranges for the SBSOD, SOT, and PSS
# SBSOD (possible range from 1 to 7)
sbsod_stats <- sbsod_perceivedStress_data %>%
  summarise(
    Mean = mean(sbsod_mean, na.rm = TRUE),
    SD = sd(sbsod_mean, na.rm = TRUE),
    Min = min(sbsod_mean, na.rm = TRUE),
    Max = max(sbsod_mean, na.rm = TRUE)
  )

# SOT (possible range from 0 to 180)
SOT_stats <- sot_data %>%
  summarise(
    Mean = mean(SOT_average_angular_error, na.rm = TRUE),
    SD = sd(SOT_average_angular_error, na.rm = TRUE),
    Min = min(SOT_average_angular_error, na.rm = TRUE),
    Max = max(SOT_average_angular_error, na.rm = TRUE)
  )

# Perceived stress score (possible range from 0 to 40)
pss_stats <- sbsod_perceivedStress_data %>%
  summarise(
    Mean = mean(perceivedStress_totalScore, na.rm = TRUE),
    SD = sd(perceivedStress_totalScore, na.rm = TRUE),
    Min = min(perceivedStress_totalScore, na.rm = TRUE),
    Max = max(perceivedStress_totalScore, na.rm = TRUE)
  )

# Add participant mean scores for each survey measure to the demographic data
demog_data <- demog_data %>%
  left_join(
    sbsod_perceivedStress_data %>% select(subjectID, sbsod_mean, perceivedStress_totalScore),
    by = "subjectID"
  )
demog_data <- demog_data %>%
  left_join(
    sot_data %>% select(subjectID, SOT_average_angular_error),
    by = "subjectID"
  )

# Now compute the correlations between the three measures
cor_data <- demog_data %>%
  select(sbsod_mean, SOT_average_angular_error, perceivedStress_totalScore)

cor_results <- rcorr(
  as.matrix(cor_data),
  type = "pearson"
)

# Correlations
round(cor_results$r, 2)

# P-values
round(cor_results$P, 3)

# Sample sizes
cor_results$n


# reset working directory to import cortisol
setwd("D:/UA_stuff/seagate_hard_drive/Nav Stress Data/Salimetrics reports") # from hard drive

# Read in saliva data 
samples_allCort <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "columnsNames")
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

samples_allCort$cort_nmol_L <- samples_allCort$mean_cort*276
samples9Data$cort_nmol_L <- samples9Data$mean_cort*276
samples12Data$cort_nmol_L <- samples12Data$mean_cort*276

all_data <- rbind(samples9Data, samples12Data)

# get baseline cortisol for each person
baseline_cort <- samples_allCort %>%
  filter(time == "pre") %>%
  group_by(subjNum) %>%
  dplyr::summarize(
    count = n(),
    mean_cort = mean(cort_nmol_L),
    sd_cort = sd(cort_nmol_L)
  )

# get learning trials to one value per subject
learningTrialsSummary <- learningTrials %>%
  group_by(subjectID) %>%
  dplyr::summarize(
    count = n(),
    avg_1stRecreation = mean(corMinusIncor1, na.rm = TRUE)
  )

# Pair down sbsod and perceived stress to just the necessary columns
sbsod_pss_data <- sbsod_perceivedStress_data %>%
  select(c(Participant.ID, sbsod_mean, perceivedStress_totalScore))

# Pair down nav trials to just the control condition
nav_ctrl_data <- nav_data %>%
  filter(condition == "ctrl")
# Make the nav dataset wide so there is only one row per subject
nav_ctrl_wide <- nav_ctrl_data %>%
  select(!sd_grid_prop) %>%
  pivot_wider(
    names_from = grid_type,
    values_from = mean_grid_prop)

# Rename ID column in different dataframes to "subjNum"
learningTrialsSummary <- learningTrialsSummary %>%
  rename(subjNum = subjectID)
nav_ctrl_wide <- nav_ctrl_wide %>%
  rename(subjNum = subjectID)
sbsod_pss_data <- sbsod_pss_data %>%
  rename(subjNum = Participant.ID)
sot_data <- sot_data %>%
  rename(subjNum = subjectID)

# Combine the dataframes: Learning summary and nav trials, AUC and baseline cort, perceived stress, SBSOD, SOT
one_per_subj <- full_join(baseline_cort, auc_data, by = "subjNum")
one_per_subj <- full_join(one_per_subj, sbsod_pss_data, by = "subjNum")
one_per_subj <- full_join(one_per_subj, sot_data, by = "subjNum")
one_per_subj <- full_join(one_per_subj, learningTrialsSummary, by = "subjNum")
one_per_subj <- full_join(one_per_subj, nav_ctrl_wide, by = "subjNum")

# Make a correlation matrix
# Select the variables of interest
corr_data <- one_per_subj %>%
  select(
    mean_cort,
    sbsod_mean,
    perceivedStress_totalScore,
    SOT_average_angular_error,
    avg_1stRecreation,
    inner_proportion,
    outer_proportion,
    novel_proportion)

# Calculate the correlation matrix
cor_matrix <- corr.test(
  corr_data,
  method = "pearson",
  use = "pairwise",
  adjust = "holm"
)

correlation(
  corr_data,
  method = "pearson",
  missing = "keep_pairwise"
)

# Plot
ggplot(corr_df, aes(x = Var2, y = Var1)) +
  geom_tile(
    aes(fill = sig),
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = label),
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "white",
      "TRUE" = "#9ECAE1"
    ),
    guide = "none"
  ) +
  scale_x_discrete(limits = colnames(cor_matrix$r)) +
  scale_y_discrete(limits = rev(colnames(cor_matrix$r))) +
  coord_fixed() +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Is baseline cortisol related to perceived stress score? - no, not sig
cor.test(bigMergeData$mean_cort, bigMergeData$perceivedStress_totalScore)
plot(bigMergeData$mean_cort, bigMergeData$perceivedStress_totalScore)

# Is Excess path correlated with sbsod score? Yes sig
cor.test(bigMergeData$sbsod_mean, bigMergeData$mean_excessPath) # p = 2.798e-07

sp <- ggscatter(bigMergeData, x = "sbsod_mean", y = "mean_excessPath", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Santa Barbara Sense of Direction Score",
                ylab = "Avg Excess Path") + 
  stat_cor(method = "pearson", label.x = 4, label.y = 700)

#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/sbsod_ExcessPathCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
sp
#dev.off()

# Is 1st recreation score correlated with excess path?
# In other words, if you learned worse, do you have more excess path? Yes, sig

cor.test(bigMergeData$avg_1stRecreation, bigMergeData$mean_excessPath) # p = 1.414e-10

s <- ggscatter(bigMergeData, x = "avg_1stRecreation", y = "mean_excessPath", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Path Recreation Accuracy (%)",
                ylab = "Avg Excess Path") + 
  stat_cor(method = "pearson", label.x = .4565, label.y = 750)

#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/recreationAccuracy_ExcessPathCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
s
#dev.off()










