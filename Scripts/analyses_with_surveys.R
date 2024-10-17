# Correlations with surveys

# Santa Barbara Sense of Direction 
# Perceived stress survey

library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

rm(list = ls())

##### Read in data
setwd("E:/Nav Stress Data/surveys/") # set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress shortcuts") # for developing

sbsod_perceivedStress_data <- read.csv("sbsod_perceviedStress_scored.csv") # read in survey file
avgExcessPathSubj <- read.csv("avgExcessPathSubj.csv") # read in excess path file
learningTrials <- read.csv("learningTrialsBigMergeData.csv") # read in the learning file)

# reset working directory to import cortisol
setwd("E:/Nav Stress Data/Salimetrics reports") # from hard drive

# Read in saliva data 
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

samples9Data$cort_nmol_L <- samples9Data$mean_cort*276
samples12Data$cort_nmol_L <- samples12Data$mean_cort*276

all_data <- rbind(samples9Data, samples12Data)

# get baseline cortisol for each person
allTimes_cort <- all_data %>%
  group_by(subjNum, time) %>%
  summarize(
    mean_cort = mean(cort_nmol_L),
    sd_cort = sd(cort_nmol_L)
  )

baseline_cort <- subset(allTimes_cort, time == "pre")
baseline_cort <- baseline_cort %>% 
  rename(Participant.ID = subjNum)

# Rename excess path subject ID column
avgExcessPathSubj <- avgExcessPathSubj %>%
  rename(Participant.ID = subjectID)

# get learning trials to one value per subject
learningTrialsSummary <- learningTrials %>%
  group_by(subjectID) %>%
  summarize(
    avg_1stRecreation = mean(corMinusIncor1, na.rm = TRUE)
  )
# rename the subject ID column
learningTrialsSummary <- learningTrialsSummary %>%
  rename(Participant.ID = subjectID)

# combine the dataframes
bigMergeData <- merge(sbsod_perceivedStress_data, baseline_cort, by = "Participant.ID")
bigMergeData <- merge(bigMergeData, avgExcessPathSubj, by = "Participant.ID")
bigMergeData <- merge(bigMergeData, learningTrialsSummary, by = "Participant.ID")

# Is baseline cortisol related to perceived stress score? - no, not sig
cor.test(cort_sbsod_percStress$mean_cort, cort_sbsod_percStress$perceivedStress_totalScore)
plot(cort_sbsod_percStress$mean_cort, cort_sbsod_percStress$perceivedStress_totalScore)

# Is Excess path correlated with sbsod score?
cor.test(bigMergeData$sbsod_mean, bigMergeData$mean_excessPath)

sp <- ggscatter(bigMergeData, x = "sbsod_mean", y = "mean_excessPath", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Santa Barbara Sense of Direction Score",
                ylab = "Avg Excess Path") + 
  stat_cor(method = "pearson", label.x = 4, label.y = 700)

jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/sbsod_ExcessPathCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
sp
dev.off()

# Is 1st recreation score correlated with excess path?
# In other words, if you learned worse, do you have more excess path?

cor.test(bigMergeData$avg_1stRecreation, bigMergeData$mean_excessPath)

s <- ggscatter(bigMergeData, x = "avg_1stRecreation", y = "mean_excessPath", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Path Recreation Accuracy (%)",
                ylab = "Avg Excess Path") + 
  stat_cor(method = "pearson", label.x = .4565, label.y = 750)

jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/recreationAccuracy_ExcessPathCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
s
dev.off()










