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
library(ARTool)
library(lme4)
library(dplyr)

rm(list = ls())

##### Read in data
#setwd("D:/Nav Stress Data/") # set working directory
setwd("D:/UA_stuff/seagate_hard_drive/Nav Stress Data/")

inputData <- read.csv("combined_recreatePathsLogData.csv") # read in file

# get rid of participants 16 and 22 because they don't have complete data
inputData <- inputData %>%
  filter(!subjectID %in% c(16,22))

# make a copy to work with
myData <- inputData

# add column with percentages of how much of the path participants recreated (not including grids shared between inner and outer paths)
# City 1 <- outer: 124; inner: 110, novel: 441
# City 2 <- outer: 127; inner: 107, novel: 441
# City 3 <- outer: 136; inner: 111, novel: 440

# calculate the percentage of grid overlap for outer and inner paths
# total grid numbers have been counted and don't include grids shared between inner and outer paths
for (i in 1:nrow(myData)){
  if (myData$city[i] == "city1"){
    myData$percent_grid_overlap_outer[i] <- myData$grid_overlap_outer[i]/124
    myData$percent_grid_overlap_inner[i] <- myData$grid_overlap_inner[i]/110
    myData$percent_grid_overlap_novel[i] <- myData$novel_grids[i]/441
  } else if (myData$city[i] == "city2"){
    myData$percent_grid_overlap_outer[i] <- myData$grid_overlap_outer[i]/127
    myData$percent_grid_overlap_inner[i] <- myData$grid_overlap_inner[i]/107
    myData$percent_grid_overlap_novel[i] <- myData$novel_grids[i]/441
  } else if (myData$city[i] == "city3"){
    myData$percent_grid_overlap_outer[i] <- myData$grid_overlap_outer[i]/136
    myData$percent_grid_overlap_inner[i] <- myData$grid_overlap_inner[i]/111
    myData$percent_grid_overlap_novel[i] <- myData$novel_grids[i]/440
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

combo_data <- rbind(outer_df, inner_df)

# make a dataset to graph more familiar and less familiar path correct percentage
graphData <- subset(combo_data, trialname == "recreatePath1" | trialname == "recreatePath2")
# make a column to label the more/less familiar path
for (j in 1:nrow(graphData)){
  if(graphData$moreFamiliarPath[j] == graphData$path_recreated[j]){
    graphData$path_recreated_cat[j] <- "moreFamPath"
  } else {graphData$path_recreated_cat[j] <- "lessFamPath"}
}
# make a factor
graphData$path_recreated_cat <- as.factor(graphData$path_recreated_cat)

# another column to total the incorrect path and novel grids so actual total incorrect
graphData$incorrect_pathANDNovel <- graphData$incorrect_recreation_percent + graphData$percent_grid_overlap_novel

# make a column for adjusted percent corr
graphData$adj_percent_correct <- (graphData$correct_recreation_percent - graphData$incorrect_recreation_percent - graphData$percent_grid_overlap_novel)

# summaries by subject and path_recreate_cat
subjTable <- graphData %>%
  group_by(subjectID) %>%
  summarize(
    count = n(), 
    mean_correct_recreation = mean(correct_recreation_percent),
    mean_incorrect_pathANDNovel = mean(incorrect_pathANDNovel), 
    mean_adj_percent_correct = mean(adj_percent_correct)
  )

# mean and sd correct only per participant
mean(subjTable$mean_correct_recreation) # 0.752
sd(subjTable$mean_correct_recreation) # 0.189

# mean and sd incorrect only per participant
mean(subjTable$mean_incorrect_pathANDNovel) # 0.203
sd(subjTable$mean_incorrect_pathANDNovel) # 0.157

# mean and sd adj percent correct per participant
mean(subjTable$mean_adj_percent_correct) # 0.550
sd(subjTable$mean_adj_percent_correct) # 0.323

sumTable <- graphData %>%
  group_by(subjectID, path_recreated_cat) %>%
  summarize(
    count = n(), 
    mean_correct_recreation = mean(correct_recreation_percent),
    mean_incorrect_pathANDNovel = mean(incorrect_pathANDNovel), 
    mean_adj_percent_correct = mean(adj_percent_correct)
  )

# mean and sd adj percent correct per participant
means <- sumTable %>%
  group_by(path_recreated_cat) %>%
  summarize(
    mean_adj_correct = mean(mean_adj_percent_correct), 
    sd_adj_correct = sd(mean_adj_percent_correct)
  )

mean(graphData$adj_percent_correct) 
sd(graphData$adj_percent_correct)

hist(graphData$adj_percent_correct) # data are skewed to the left

# Check how many people had negative numbers for adj percent correct
negatives <- sum(graphData$adj_percent_correct < 0)

# Check normality
hist(graphData$adj_percent_correct)
hist(graphData$correct_recreation_percent)
hist(graphData$incorrect_recreation_percent)
hist(graphData$percent_grid_overlap_novel)

# one data point each per participant per familiarity level
adj_correct_plot <- graphData %>%
  group_by(subjectID, path_recreated_cat) %>%
  summarize(
    mean_adj_percent_correct = mean(adj_percent_correct, na.rm = TRUE)
  )

# filter out participants 22 adn 26 for not having complete cases
filtered_adj_percent_correct <- adj_correct_plot %>%
  filter(!subjectID %in% c(22,26))

hist(filtered_adj_percent_correct$mean_adj_percent_correct)

# MANUSCRIPT PIC
overlap_fam_plot <- ggplot(filtered_adj_percent_correct, aes(x = path_recreated_cat, y = mean_adj_percent_correct)) + 
  geom_violin() + geom_point() + geom_line(aes(group = subjectID), color = "gray") +
  stat_summary(aes(group = path_recreated_cat), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  theme_classic() +
  labs(x = "Familiarity", y = "Adjusted Percent Correct", fill = "Recreation Category") +
  scale_x_discrete(labels = c("Less Familiar", "More Familiar")) + 
  scale_fill_discrete(name = "Recreated Category", labels = c("Overlap with recreated path", "Overlap with nonrecreated path"), type = c("cyan3", "salmon")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#jpeg("D:/UA_stuff/seagate_hard_drive/Nav Stress Data/manuscript_pics/learning_fam_adjPercent.jpeg", width = 6, height = 5, units = 'in', res = 500)
overlap_fam_plot
#dev.off()

# check if the differences between the pairs is normally distributed
diffs_df <- pivot_wider(filtered_adj_percent_correct, names_from = path_recreated_cat, values_from = mean_adj_percent_correct)
diffs_df$diffs <- diffs_df$moreFamPath - diffs_df$lessFamPath
hist(diffs_df$diffs)

shapiro.test(diffs_df$diffs) # not sig, the diffs are normally distributed

t.test(mean_adj_percent_correct ~ path_recreated_cat, paired = TRUE, data = filtered_adj_percent_correct) # sig

# make the graph for each participant
subjGraph <- graphData %>%
  select(subjectID, correct_recreation_percent, incorrect_pathANDNovel, adj_percent_correct)
# make long
subjGraph_long <- pivot_longer(!subjectID, names_to = "type", values_to = "percent", data = subjGraph)

# MANUSCRIPT PIC
indiv_diffs_plot2 <- ggplot(graphData, aes(x = subjectID)) +
  geom_boxplot(aes(y = correct_recreation_percent, fill = "steelblue")) +
  geom_boxplot(aes(y = incorrect_pathANDNovel, fill = "lightpink")) +
  labs(x = "Individual Participants", y = "Average Grid Overlap Percent") +
  scale_fill_discrete(name = "Recreated Category", labels = c("Overlap with nonrecreated path", "Overlap with recreated path")) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 13), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        legend.position = "top")
#jpeg("D:/UA_stuff/seagate_hard_drive/Nav Stress Data/manuscript_pics/learning_indivDiffs.jpeg", width = 8.5, height = 6, units = 'in', res = 500)
indiv_diffs_plot2
#dev.off()

# MANUSCRIPT PIC
subj_percent_plot <- ggplot(subjTable_long, aes(x = subjectID, y = percent, color = type)) + 
  geom_violin() + geom_jitter() + geom_line(aes(group = subjectID), color = "gray") +
  theme_classic() +
  labs(x = "Familiarity Level", y = "Average Grid Overlap Percent", fill = "Recreation Category") +
  scale_fill_discrete(name = "Recreated Category", labels = c("Overlap with recreated path", "Overlap with nonrecreated path"), type = c("cyan3", "salmon")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#jpeg("D:/Nav Stress Data/dissertation/pics/learning_fam_adjPercent.jpeg", width = 6, height = 5, units = 'in', res = 500)
subj_percent_plot
#dev.off()

##### stats for the plot
good <- plot_graph %>%
  filter(percent_cat == "avg_corr_recreate_percent")
hist(good$percentage)

bad <- plot_graph %>%
  filter(percent_cat == "avg_incorr_recreate_percent")
hist(bad$percentage)

##### 2-way repeated-measures ANOVA walk view or maybe linear mixed effects model
# summary stats used in 2way rep ANOVA
aov_means <- plot_graph %>%
  group_by(path_recreated_cat, percent_cat) %>%
  get_summary_stats(percentage, type = "mean_sd")

# organize the data for the test
aov_data <- plot_graph %>%
  group_by(subjectID, path_recreated_cat, percent_cat) %>%
  summarize(
    mean_percent = mean(percentage, na.rm = TRUE),
  )
aov_data <- as_tibble(aov_data)

# check for outliers - 3 but not extreme so they stay in
outliers <- aov_data %>%
  group_by(path_recreated_cat, percent_cat) %>%
  identify_outliers(mean_percent)

# check normality - they are all significant but not that bad
normality <- aov_data %>%
  group_by(path_recreated_cat, percent_cat) %>%
  shapiro_test(mean_percent)

# qq plot - not great but ok
ggqqplot(aov_data, "mean_percent", ggtheme = theme_bw()) +
  facet_grid(path_recreated_cat ~ percent_cat, labeller = "label_both")

# test
withinTest <- anova_test(data = aov_data, dv = mean_percent, wid = subjectID,
                         within = c(path_recreated_cat, percent_cat))
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

# graph of correct recreation percent
ggplot(combo_data, aes(x = subjectID, y = correct_recreation_percent)) +
  geom_boxplot(color = "blue", fill = "steelblue", alpha = 0.2) +
  theme_classic()

# graph of incorrect recreation percent
ggplot(combo_data, aes(x = subjectID, y = incorrect_recreation_percent)) +
  geom_boxplot(color = "red", fill = "lightpink", alpha = 0.2)

# MANUSCRIPT PIC
# this graph is a little busy - shows correct and incorrect recreation percentage
indiv_diffs_plot <- ggplot(combo_data, aes(x = subjectID)) +
  geom_boxplot(aes(y = correct_recreation_percent, fill = "steelblue")) +
  geom_boxplot(aes(y = incorrect_recreation_percent, fill = "lightpink")) +
  labs(x = "Participant Number", y = "Average Grid Overlap Percent") +
  scale_fill_discrete(name = "Recreated Category", labels = c("Overlap with nonrecreated path", "Overlap with recreated path")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Dissertation/pics/learning_indivDiffs.jpeg", width = 12, height = 6, units = 'in', res = 500)
indiv_diffs_plot
#dev.off()

# Make the graph again with only the first two recreated paths
first_recreation <- combo_data %>%
  filter(trialname == "recreatePath1" | trialname == "recreatePath2")
# only the first and second recreation 
indiv_diffs_plot2 <- ggplot(first_recreation, aes(x = subjectID)) +
  geom_boxplot(aes(y = correct_recreation_percent, fill = "steelblue")) +
  geom_boxplot(aes(y = incorrect_recreation_percent, fill = "lightpink")) +
  labs(x = "Participant Number", y = "Average Grid Overlap Percent") +
  scale_fill_discrete(name = "Recreated Category", labels = c("Overlap with nonrecreated path", "Overlap with recreated path")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Dissertation/pics/learning_indivDiffs2.jpeg", width = 12, height = 6, units = 'in', res = 500)
indiv_diffs_plot2
#dev.off()

##### Find the bad navigators
# Bad navigators will be defined as less than 50% knowledge of the correct paths
# Get each person's correct and incorrect percent
good_bad_navs <- combo_data %>%
  group_by(subjectID) %>%
  summarize(
    mean_correct_overlap = mean(correct_recreation_percent), 
    mean_incorrect_overlap = mean(incorrect_recreation_percent)
  )
  
# Add a label for each person as bad (0-50), med (50-75), and good (75-100)
good_bad_navs <- good_bad_navs %>%
  mutate(label = case_when(
    mean_correct_overlap >= 0 & mean_correct_overlap <= 0.5 ~ "bad", 
    mean_correct_overlap > 0.5 & mean_correct_overlap <= 0.75 ~ "good",
    mean_correct_overlap >= 0.75 & mean_correct_overlap <= 1 ~ "great"
  ))

# Write to a csv
#write.csv(good_bad_navs, "E:/Nav Stress Data/good_bad_nav_labels.csv", row.names = FALSE)

plot_data <- good_bad_navs %>%
  group_by(label) %>%
  summarize(
    count = n(), 
    mean_correct = mean(mean_correct_overlap), 
    mean_incorrect = mean(mean_incorrect_overlap)
  )
ggplot(plot_data, aes(x = label, y = mean_correct)) +
  geom_boxplot()

# do people remember at the end too?
# make a dataset to graph more familiar and less familiar path correct percentage
graphData2 <- subset(combo_data, trialname == "recreatePath3" | trialname == "recreatePath4")
# make a column to label the more/less familiar path
for (j in 1:nrow(graphData2)){
  if(graphData2$moreFamiliarPath[j] == graphData2$path_recreated[j]){
    graphData2$path_recreated_cat[j] <- "moreFamPath"
  } else {graphData2$path_recreated_cat[j] <- "lessFamPath"}
}

# make graph with more and less familiar path categories and correct percentage
ggplot(graphData2, aes(x = subjectID, y = correct_recreation_percent, color = path_recreated_cat)) +
  geom_point(size = 3)

# find the average for each person
plot_graph2 <- graphData2 %>%
  group_by(subjectID, path_recreated_cat) %>%
  summarise(
    avg_corr_recreate_percent = mean(correct_recreation_percent, na.rm = TRUE),
    sd_corr_recreate_percent = sd(correct_recreation_percent, na.rm = TRUE),
    avg_incorr_recreate_percent = mean(incorrect_recreation_percent, na.rm = TRUE),
    sd_incorr_recreate_percent = sd(incorrect_recreation_percent, na.rm = TRUE)
  )

# gather corr and incorr categories together
# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("avg_corr_recreate_percent", "avg_incorr_recreate_percent")

# gather the data for grid numbers
plot_graph2 <- gather(plot_graph2, key = percent_cat, value = percentage, gathered_columns, factor_key = TRUE)

# Graph for iNAV
tick_labels <- c("Less Familiar", "More Familiar")

p <- ggplot(plot_graph2, aes(x = path_recreated_cat, y = percentage, fill = percent_cat)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  labs(x = "Path Type Recreated", y = "Avg Percent Overlap", fill = "Category") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "", labels = c("Correctly Recreated", "Incorrectly Recreated")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/recreatedMoreLessFam.jpeg", width = 6.5, height = 5.75, units = 'in', res = 500)
p
#dev.off()







# does the first recreated path correlate with the second recreated path

outer_1st <- subset(graphData, path_recreated == "outer" & (trialname == "recreatePath1" | trialname == "recreatePath2"))
inner_1st <- subset(graphData, path_recreated == "inner" & (trialname == "recreatePath1" | trialname == "recreatePath2"))

outer_2nd <- subset(graphData2, path_recreated == "outer" & (trialname == "recreatePath3" | trialname == "recreatePath4"))
inner_2nd <- subset(graphData2, path_recreated == "inner" & (trialname == "recreatePath3" | trialname == "recreatePath4"))

set1 <- merge(inner_1st, outer_1st, by=c("subjectID", "city"))
set2 <- merge(inner_2nd, outer_2nd, by=c("subjectID", "city"))
mergeDataset <- merge(set1, set2, by=c("subjectID", "city"))

names(mergeDataset) <- sub("\\.x\\.x$", ".i1", names(mergeDataset))
names(mergeDataset) <- sub("\\.y\\.x$", ".o1", names(mergeDataset))
names(mergeDataset) <- sub("\\.x\\.y$", ".i2", names(mergeDataset))
names(mergeDataset) <- sub("\\.y\\.y$", ".o2", names(mergeDataset))

# overall, the inner and outer routes are highly correlated and at the same level
cor.test(mergeDataset$correct_recreation_percent.i1, mergeDataset$correct_recreation_percent.i2) # 0.89
cor.test(mergeDataset$correct_recreation_percent.o1, mergeDataset$correct_recreation_percent.o2) # 0.86

plot(mergeDataset$correct_recreation_percent.i1, mergeDataset$correct_recreation_percent.i2)
plot(mergeDataset$correct_recreation_percent.o1, mergeDataset$correct_recreation_percent.o2)

# now let's split this by familiar path to see if the more familiar path is more highly correlated than the less familiar path
i1i2 <- mergeDataset[, c(1:19, 37:53)]
o1o2 <- mergeDataset[, c(1:2, 20:36, 54:70)]

moreFam_i1i2 <- subset(i1i2, path_recreated_cat.i1 == "moreFamPath")
lessFam_i1i2 <- subset(i1i2, path_recreated_cat.i1 == "lessFamPath")

moreFam_o1o2 <- subset(o1o2, path_recreated_cat.o1 == "moreFamPath")
lessFam_o1o2 <- subset(o1o2, path_recreated_cat.o1 == "lessFamPath")

cor.test(moreFam_o1o2$correct_recreation_percent.o1, moreFam_o1o2$correct_recreation_percent.o2) # 0.74
cor.test(lessFam_o1o2$correct_recreation_percent.o1, lessFam_o1o2$correct_recreation_percent.o2) # 0.88
cor.test(moreFam_i1i2$correct_recreation_percent.i1, moreFam_i1i2$correct_recreation_percent.i2) # 0.89
cor.test(lessFam_i1i2$correct_recreation_percent.i1, lessFam_i1i2$correct_recreation_percent.i2) # 0.90

plot(moreFam_o1o2$correct_recreation_percent.o1, moreFam_o1o2$correct_recreation_percent.o2, xlim = c(0,1), ylim = c(0,1))
plot(lessFam_o1o2$correct_recreation_percent.o1, lessFam_o1o2$correct_recreation_percent.o2, xlim = c(0,1), ylim = c(0,1))
plot(moreFam_i1i2$correct_recreation_percent.i1, moreFam_i1i2$correct_recreation_percent.i2, xlim = c(0,1), ylim = c(0,1))
plot(lessFam_i1i2$correct_recreation_percent.i1, lessFam_i1i2$correct_recreation_percent.i2, xlim = c(0,1), ylim = c(0,1))

names(moreFam_i1i2) <- sub("\\.i", ".", names(moreFam_i1i2))
names(moreFam_o1o2) <- sub("\\.o", ".", names(moreFam_o1o2))

names(lessFam_i1i2) <- sub("\\.i", ".", names(lessFam_i1i2))
names(lessFam_o1o2) <- sub("\\.o", ".", names(lessFam_o1o2))

moreFamRecreate <- rbind(moreFam_i1i2, moreFam_o1o2)
lessFamRecreate <- rbind(lessFam_i1i2, lessFam_o1o2)

cor.test(moreFamRecreate$correct_recreation_percent.1, moreFamRecreate$correct_recreation_percent.2) # 0.86
cor.test(lessFamRecreate$correct_recreation_percent.1, lessFamRecreate$correct_recreation_percent.2) # 0.89

plot(moreFamRecreate$correct_recreation_percent.1, moreFamRecreate$correct_recreation_percent.2)
plot(lessFamRecreate$correct_recreation_percent.1, lessFamRecreate$correct_recreation_percent.2)




# just do one correlation with first path correlating with second path
names(moreFamRecreate) <- sub("\\.i", "", names(moreFamRecreate))
names(lessFamRecreate) <- sub("\\.o", "", names(lessFamRecreate))

bigMergeData <- rbind(moreFamRecreate, lessFamRecreate)

cor.test(bigMergeData$correct_recreation_percent.1, bigMergeData$correct_recreation_percent.2)

plot(bigMergeData$correct_recreation_percent.1, bigMergeData$correct_recreation_percent.2)

# make a column of correct minus incorrect to get a measure of overall how they did
bigMergeData$corMinusIncor1 <- bigMergeData$correct_recreation_percent.1 - bigMergeData$incorrect_recreation_percent.1
bigMergeData$corMinusIncor2 <- bigMergeData$correct_recreation_percent.2 - bigMergeData$incorrect_recreation_percent.2

cor.test(bigMergeData$corMinusIncor1, bigMergeData$corMinusIncor2)

# make a column of correct minus incorrect for more and less familiar
moreFamSubset <- bigMergeData %>%
  filter(path_recreated_cat.1 == "moreFamPath")
lessFamSubset <- bigMergeData %>%
  filter(path_recreated_cat.1 == "lessFamPath")

# graph for iNAV
sp <- ggscatter(bigMergeData, x = "corMinusIncor1", y = "corMinusIncor2", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Initial Path Recreation Accuracy",
                ylab = "End Path Recreation Accuracy") + 
  stat_cor(method = "pearson", label.x = 0, label.y = .85)

#jpeg("E:/Nav Stress Data/dissertation/pics/recreation1and2cor.jpeg", width = 6, height = 5, units = 'in', res = 500)
sp
#dev.off()

# graphs for more and less familiar

sp <- ggscatter(moreFamSubset, x = "corMinusIncor1", y = "corMinusIncor2", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Initial Path Recreation Accuracy",
                ylab = "End Path Recreation Accuracy", 
                title = "More Familiar Path") + 
  stat_cor(method = "pearson", label.x = 0.10, label.y = .85) +
  theme(plot.title = element_text(hjust = 0.5))

#jpeg("E:/Nav Stress Data/dissertation/pics/recreation1and2corMOREfam.jpeg", width = 6, height = 3, units = 'in', res = 500)
sp
#dev.off()

sp <- ggscatter(lessFamSubset, x = "corMinusIncor1", y = "corMinusIncor2", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Initial Path Recreation Accuracy",
                ylab = "End Path Recreation Accuracy", 
                title = "Less Familiar Path") + 
  stat_cor(method = "pearson", label.x = 0.05, label.y = .85) +
  theme(plot.title = element_text(hjust = 0.5))

#jpeg("E:/Nav Stress Data/dissertation/pics/recreation1and2corLESSfam.jpeg", width = 6, height = 3, units = 'in', res = 500)
sp
#dev.off()

res.lm <- lm(data = bigMergeData, corMinusIncor2 ~ corMinusIncor1 * path_recreated_cat.1)
summary(res.lm)

# Write bigMergeData to a new csv file
#write.csv(bigMergeData, "E:/Nav Stress Data/surveys/learningTrialsBigMergeData.csv", row.names = FALSE)



navPlot <- ggplot(NO_nav_summary2, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  labs(x = "Grid Type", y = "Median Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/navMoreLessFam.jpeg", width = 9.5, height = 5.75, units = 'in', res = 500)
navPlot
#dev.off()







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
    mean_gridNum = mean(grid_number, na.rm = TRUE),
    sd_gridNum = sd(grid_number, na.rm = TRUE),
    median_gridNum = median(grid_number, na.rm = TRUE), 
    IQR_gridNum = IQR(grid_number, na.rm = TRUE)
  )

# This graph for Kailee
wrap_labels <- c("Inner Path Recreated", "Outer Path Recreated")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Outer Path", "Inner Path", "Novel Grids")

q <- ggplot(grid_type_table, aes(x = grid_type, y = median_gridNum, fill = moreFamiliarPath)) +
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  facet_wrap(vars(path_recreated), labeller = labeller(path_recreated = wrap_labels)) +
  labs(x = "Grid Type Overlap", y = "Median Grid Number", fill = "More Familiar Path") +
  scale_x_discrete(labels = tick_labels) + scale_fill_discrete(name = "More Familiar Path", labels = c("Inner", "Outer"))
  
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/gridTypeOverlapLearning.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
q
#dev.off()



####################


# name the columns you want to gather, the other columns will remain there
percent_gathered_columns <- c("percent_grid_overlap_outer", "percent_grid_overlap_inner")

# gather the data for grid percentages
longPercentGrid <- gather(myData, key = grid_type, value = grid_percent, percent_gathered_columns, factor_key = TRUE)

# gather the correct and incorrect trials to plot them and do stats
long_corr_incorr <- gather(myData, key = trial_type, value = recreation_percent, c(correct_recreation_percent, incorrect_recreation_percent))

hist(longPercentGrid$grid_percent) # bimodal distribution because of the more and less familiar paths
hist(myData$correct_recreation_percent)
hist(myData$incorrect_recreation_percent)

means <- longPercentGrid %>% 
  group_by(path_recreated, grid_type) %>%
  summarize(
    mean_gridPercent = mean(grid_percent, na.rm = TRUE),
    sd_gridPercent = sd(grid_percent, na.rm = TRUE)
  )

bxp <- ggboxplot(
  longPercentGrid, x = "path_recreated", y = "grid_percent", color = "grid_type"
)
bxp

# identify outliers: this identifies outliers but if you 2.5 sd away from the mean, there really aren't outliers
outliers <- longPercentGrid %>%
  group_by(path_recreated, grid_type) %>%
  identify_outliers(grid_percent)

meanGridPercent <- mean(longPercentGrid$grid_percent, na.rm = TRUE)
sdGridPercent <- sd(longPercentGrid$grid_percent, na.rm = TRUE)

# get rid of outliers - actually only got rid of the NAs
longPercentGrid_NO <- longPercentGrid %>%
  filter(grid_percent <= (meanGridPercent + (2.5*sdGridPercent)))

# redo the plot even though it will be the same
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

# try ARTool analyses here

m <- art(grid_percent ~ grid_type*moreFamiliarPath + Error(condition), data = longPercentGrid_NO)
summary(m) # summary indicates that maybe I shouldn't use this analysis
anova(m)


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

# test to see if the medians are different: p = .02, they are different
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

# now the test: p = 0.214, the medians are not different could indicate 
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


##### Linear model 

lm_results <- lm(data = graphData, correct_recreation_percent ~ path_recreated_cat*path_recreated)
summary(lm_results)

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

# graph that connects each person's data points
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
  facet_wrap(vars(condition)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID))

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











