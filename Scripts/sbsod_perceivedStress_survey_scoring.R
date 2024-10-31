# Script to separate and score Santa Barbara Sense of Direction Scale and Perceived Stress Survey

# participants 4, 16, and 22

library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

rm(list = ls())

# Set working directory and file name
setwd("E:/Nav Stress Data/surveys")

# import the data
originalData <- read.csv("sbsod_perceivedStress.csv")

# make a copy to work with
myData <- originalData

# delete row 2 because we don't need it
myData <- myData[-2,]

#################### Santa Barbara Sense of Direction ####################

# create a subset of data for the Santa Barbara Sense of Direction survey
sbsod <- myData[, c(21:35, 20)]

# make row 1 the column names
colnames(sbsod) <- sbsod[1,]

# delete row 1
sbsod <- sbsod[-1,]

# make all the numbers numeric
sbsod <- as.data.frame(lapply(sbsod, function(x) as.numeric(as.character(x))))

# reverse code survey questions 1, 3, 4, 5, 7, 9, and 14 - this is also the column number
# make a new dataframe for the reversed scores
sbsod_rev <- sbsod

# define the reverse score mapping
sbsod_reverse_score <- function(x) {
  return(8-x)
}

# specify columns to reverse score
columns_to_reverse <- c(1,3,4,5,7,9,14)

# reverse the scores
sbsod_rev[columns_to_reverse] <- lapply(sbsod_rev[columns_to_reverse], sbsod_reverse_score)

# calculate mean for SBSOD
sbsod_rev$sbsod_mean <- rowMeans(sbsod_rev[,-16], na.rm = TRUE)

#################### Perceived Stress Scale ####################

# create a subset of data for the perceived stress scale
perceivedStress <- myData[c(36:45, 20)]

# make row 1 the column names
colnames(perceivedStress) <- perceivedStress[1,]

# delete row 1
perceivedStress <- perceivedStress[-1,]

# define mapping from categorical to numeric values
perceivedStress_mapping <- c("Never" = 0, "Almost Never" = 1, "Sometimes" = 2, "Fairly Often" = 3, "Very Often" = 4)

# replace categorical with numeric values
perceivedStress_nums <- perceivedStress %>%
  mutate_at(vars(1:10), ~ perceivedStress_mapping[.])

# make all the numbers numeric
perceivedStress_nums <- as.data.frame(lapply(perceivedStress_nums, function(x) as.numeric(as.character(x))))

# make a new dataframe for the reversed scores
perceivedStress_rev <- perceivedStress_nums

# define the reverse score mapping
percStress_reverse_score <- function(x) {
  return(4-x)
}

# specify columns to reverse score
columns_to_reverse <- c(4,5,7,8)

# reverse the scores
perceivedStress_rev[columns_to_reverse] <- lapply(perceivedStress_rev[columns_to_reverse], percStress_reverse_score)

# add the scores for survey
perceivedStress_rev$perceivedStress_totalScore <- rowSums(perceivedStress_rev[,-11], na.rm = TRUE)

# merge the two data sets into one
sbsod_perceviedStress_scored <- merge(sbsod_rev, perceivedStress_rev, by = "Participant.ID") 

# Write data to a new csv file
write.csv(sbsod_perceviedStress_scored, paste0("E:/Nav Stress Data/sbsod_perceivedStress_scored.csv"), row.names = FALSE)
