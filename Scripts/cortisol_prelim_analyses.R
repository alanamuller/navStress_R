library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(magrittr)
library(dplyr)

# Start fresh by removing everything from the environment
rm(list = ls())

# Set working directory
setwd("E:/Nav Stress Data/Salimetrics reports") # from hard drive

# Enter the data 
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

all_data <- rbind(samples9Data, samples12Data)

all_data$log_cort <- log(all_data$mean_cort) # calculate log cortisol
all_data$cort_nmol_L <- all_data$mean_cort *276 # calculate mnol/L for cortisol


# Make dataframe to add a column to correct for the baseline for that condition
# Make a wide version of the data
wide_data <- pivot_wider(data = all_data, id_cols = num_cond, names_from = time, values_from = mean_cort)

# Add baseline corrected columns
wide_data$bc_post1 <- wide_data$post1 - wide_data$pre
wide_data$bc_post15 <- wide_data$post15 - wide_data$pre
wide_data$bc_post30 <- wide_data$post30 - wide_data$pre

# Make a long version of the data
long_data <- pivot_longer(data = wide_data, cols = !num_cond,names_to = "time", values_to = "mean_cort")

# Make separate dataframes for total mean cort and baseline corrected mean cort
whole_cort <- long_data %>%
  filter(time == "pre" | time == "post1" | time == "post15" | time == "post30")
whole_cort <- whole_cort %>%
  separate_wider_delim(num_cond, delim = "_", names = c("subjNum", "condition"))

bc_cort <- long_data %>%
  filter(time == "bc_post1" | time == "bc_post15" | time == "bc_post30")
bc_cort <- bc_cort %>%
  separate_wider_delim(num_cond, delim = "_", names = c("subjNum", "condition"))

# Set the order of the x-axis
level_order <- c('pre', 'post1', 'post15', 'post30')
bc_level_order <- c('bc_post1', 'bc_post15', 'bc_post30')

##### Pics of all data

# Plot actual cortisol amount with all participant separated by condition and time
ggplot(data = whole_cort, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

# Line plot with Ss's amounts connected
ggplot(data = whole_cort, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

# Plot with baseline corrected cort
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

# Line plot with Ss's amounts connected
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

##### Pics without 10, 15, 20, 23, 28 that didn't finish the cp task

# Participants to exclude
participants_to_exclude <- c(10, 15, 20, 23, 28)

# Exclude data from the specified participants
whole_cort_cpSuccess <- whole_cort %>%
  filter(!subjNum %in% participants_to_exclude)
whole_cort_cpSuccess$log_cort <- log(whole_cort_cpSuccess$mean_cort)

bc_cort_cpSuccess <- bc_cort %>%
  filter(!subjNum %in% participants_to_exclude)
bc_cort_cpSuccess$log_cort <- log(bc_cort_cpSuccess$mean_cort + 1) # plus 1 so NaNs aren't produced

# Make a separate dataframe with only complete cases
# Participants to exclude
incompletes_to_exclude <- c(1, 2, 3, 5, 6, 8, 9, 16, 22, 30)

# Exclude data from the specified participants
whole_cort_complete <- whole_cort_cpSuccess %>%
  filter(!subjNum %in% incompletes_to_exclude)

# Exclude data from the specified participants
bc_cort_complete <- bc_cort_cpSuccess %>%
  filter(!subjNum %in% incompletes_to_exclude)


# Plot actual cortisol amount with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

# Line plot with Ss's amounts connected
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

# Plot with baseline corrected cort
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

# Line plot with Ss's amounts connected
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (µg/dL)" )

########## Checking assumptions

##### There are outliers but I need all the data points for complete observations so no outliers are being deleted

##### All data

# Checking normality
ggqqplot(whole_cort_cpSuccess$mean_cort)
hist(whole_cort_cpSuccess$mean_cort)
hist(whole_cort_cpSuccess$log_cort)
ggqqplot(whole_cort_cpSuccess$log_cort)

normality_allData <- all_data %>%
  group_by(time) %>%
  shapiro_test(log_cort)
normality_allData

# anova
res.aov <- anova_test(data = whole_cort_cpSuccess, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov)

# testing simple main effects

# testing effect of condition at every time point
one.way <- whole_cort_cpSuccess %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# equal cases so pairing it will work
pwc <- whole_cort_complete %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


one.way2 <- whole_cort_cpSuccess %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# equal cases so pairing it will work
pwc2 <- whole_cort_complete %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


# Make a plot
cond.labs <- c("Cold Pressor", "Control Condition", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol" )

# Make a plot
cond.labs <- c("Cold Pressor", "Control", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
cort_graph1 <- ggplot(data = whole_cort_complete, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol" ) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17), 
        strip.text = element_text(size = 13)) +
  scale_x_discrete(labels = c("Pre", "Post1", "Post15", "Post30"))


#jpeg("C:/Users/amuller/Desktwhole_cort_complete#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/log_cortisol1.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph1
#dev.off()

########## Stats with baseline corrected cortisol ##########

# Checking normality
ggqqplot(bc_cort_complete$mean_cort)
hist(bc_cort_complete$mean_cort)
hist(bc_cort_complete$log_cort)
ggqqplot(bc_cort_complete$log_cort)

normality_bc_cort <- bc_cort_complete %>%
  group_by(time) %>%
  shapiro_test(log_cort)
normality_bc_cort

# anova
res.aov <- anova_test(data = bc_cort_complete, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov)

# testing simple main effects

# testing effect of condition at every time point
one.way <- bc_cort_complete %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# equal cases so pairing it will work
pwc <- bc_cort_complete %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


one.way2 <- bc_cort_complete %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# equal cases so pairing it will work
pwc2 <- bc_cort_complete %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2








# Checking normality
ggqqplot(bc_cort$mean_cort_change)
hist(baselineCorr_data$mean_cort_change)
hist(log(baselineCorr_data$mean_cort_change))

normality_baselineCort <- baselineCorr_data %>%
  group_by(condition, time) %>%
  shapiro_test(mean_cort_change) # normal!

res.aov <- anova_test(data = baselineCorr_data, dv = mean_cort_change, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # sig main effects, no sig interaction

# testing effect of condition at every time point
one.way5 <- baselineCorr_data %>%
  group_by(time) %>%
  anova_test(dv = mean_cort_change, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way5

# check pairwise comparisons
pwc5 <- baselineCorr_data %>%
  group_by(time) %>%
  pairwise_t_test(
    mean_cort_change ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc5

one.way6 <- baselineCorr_data %>%
  group_by(condition) %>%
  anova_test(dv = mean_cort_change, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way6

# unequal cases so pairing it won't work
pwc6 <- baselineCorr_data %>%
  group_by(condition) %>%
  pairwise_t_test(
    mean_cort_change ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc6