library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(pracma)

# Start fresh by removing everything from the environment
rm(list = ls())

# Set working directory
setwd("E:/Nav Stress Data/Salimetrics reports") # from hard drive

# Enter the data - for now only analyze the complete cases
#samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

#all_data <- rbind(samples9Data, samples12Data)

all_data <- samples12Data

all_data$cort_nmol_L <- all_data$mean_cort *276 # calculate nmol/L for cortisol
all_data$log_cort <- log(all_data$cort_nmol_L) # calculate log cortisol using nmol/L

# Make dataframe to add a column to correct for the baseline for that condition
# Make a wide version of the data
wide_data <- pivot_wider(data = all_data, id_cols = num_cond, names_from = time, values_from = cort_nmol_L)

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
whole_cort$log_cort <- log(whole_cort$mean_cort)

bc_cort <- long_data %>%
  filter(time == "bc_post1" | time == "bc_post15" | time == "bc_post30")
bc_cort <- bc_cort %>%
  separate_wider_delim(num_cond, delim = "_", names = c("subjNum", "condition"))
bc_cort$log_cort <- log(bc_cort$mean_cort)

# Set the order of the x-axis
level_order <- c('pre', 'post1', 'post15', 'post30')
bc_level_order <- c('bc_post1', 'bc_post15', 'bc_post30')

########## Checking assumptions

##### There are outliers but I need all the data points for complete observations so no outliers are being deleted

##### All data

# Checking normality
hist(whole_cort$mean_cort)
ggqqplot(whole_cort$mean_cort)
hist(whole_cort$log_cort)
ggqqplot(whole_cort$log_cort)

normality_allData <- all_data %>%
  group_by(time) %>%
  shapiro_test(log_cort)
normality_allData # all still sig but better

# anova
res.aov <- anova_test(data = whole_cort, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # time and condition*time interaction sig

# testing simple main effects

# testing effect of condition at every time point
one.way <- whole_cort %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way # post15 is sig

# equal cases so pairing it will work
pwc <- whole_cort %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc # cp and fire are sig diff at post15 and post30, nothing else

one.way2 <- whole_cort %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2 # cp is sig

# equal cases so pairing it will work
pwc2 <- whole_cort %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2 # cp, (post1 vs post15), (post15 vs post 30), (post15 vs pre), (post30 vs pre) are sig

# Make a plot
cond.labs <- c("Cold Pressor", "Control", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
cort_graph1 <- ggplot(data = whole_cort, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(color = "gray60") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  geom_line(aes(group = subjNum), color = "gray80", alpha = 0.7) +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol (nmol/L)" ) +
  theme_classic() +
  scale_x_discrete(labels = c("Pre", "Post1", "Post15", "Post30")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#jpeg("E:/Nav Stress Data/dissertation_pics/log_cortisol.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph1
#dev.off()

##### Take out the people that didn't finish the cold pressor task - SAME RESULTS AS IF THEY WERE LEFT IN THE SAMPLE

# Participants to exclude
bad_cp <- c(10, 15, 18, 20, 23, 28)

good_cp_data <- all_data %>%
  filter(!subjNum %in% bad_cp)

# Plot and ANOVA

summary_table2 <- good_cp_data %>%
  group_by(condition, time, gender) %>%
  summarize(
    count = n(),
    mean_cort_log = mean(log_cort), 
    sd_cort_log = sd(log_cort)
  )

ggplot(data = summary_table2, aes(x=factor(time, level = level_order), y = mean_cort_log, color = condition, group = condition)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~gender)

res.aov <- anova_test(data = good_cp_data, dv = log_cort, wid = subjNum, within = c(condition,time), between = gender)
get_anova_table(res.aov) # time and condition:time sig


########### AUC stuff

##### Subset raw cort data to put it in wide form
auc_cort <- whole_cort_cpSuccess %>%
  select(subjNum, condition, time, mean_cort)
auc_cort$condition_time <- paste(auc_cort$condition, auc_cort$time, sep = "_")
auc_cort <- auc_cort[,-c(2,3)]  

auc_cort_wide <- auc_cort %>%
  pivot_wider(names_from = condition_time, values_from = mean_cort)

# Initialize an empty vector to store the AUC values
auc_values_ctrl <- numeric(nrow(auc_cort_wide))
auc_values_cp <- numeric(nrow(auc_cort_wide))
auc_values_fire <- numeric(nrow(auc_cort_wide))

# Loop through each participant and condition to calculate the AUC
for (i in 1:nrow(auc_cort_wide)) {
  # Get the x and y values for the current participant and condition
  x <- c(1, 2, 3, 4)
  y <- c(auc_cort_wide$ctrl_pre[i], auc_cort_wide$ctrl_post1[i], auc_cort_wide$ctrl_post15[i], auc_cort_wide$ctrl_post30[i])
  y1 <- c(auc_cort_wide$cp_pre[i], auc_cort_wide$cp_post1[i], auc_cort_wide$cp_post15[i], auc_cort_wide$cp_post30[i])
  y2 <- c(auc_cort_wide$fire_pre[i], auc_cort_wide$fire_post1[i], auc_cort_wide$fire_post15[i], auc_cort_wide$fire_post30[i])
  
  # Calculate AUC using the trapz function
  auc_values_ctrl[i] <- trapz(x, y)
  auc_values_cp[i] <- trapz(x,y1)
  auc_values_fire[i] <- trapz(x,y2)
}

# Create a new data frame with participant IDs, conditions, and their respective AUC values
auc_table <- data.frame(
  subjNum = auc_cort_wide$subjNum,
  ctrl = auc_values_ctrl, 
  cp = auc_values_cp,
  fire = auc_values_fire
)

# Make table long
auc_table_long <- auc_table %>%
  pivot_longer(!subjNum, names_to = "condition", values_to = "auc")

# One way ANOVA
hist(auc_table_long$auc)
hist(log(auc_table_long$auc))
auc_table_long$auc_log <- log(auc_table_long$auc)

res.aov <- aov(auc_log ~ condition, data = auc_table_long)
summary(res.aov) # no sig diff

# Graph
ggplot(auc_table_long, aes(x = condition, y = auc_log)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75))

##### Now do the same for the baseline corrected data

##### Actual real data: Subset baseline corrected cort data to put it in wide form
auc_cort_bc <- bc_cort %>%
  select(subjNum, condition, time, mean_cort)

# Join condition and time column together
auc_cort_bc$condition_time <- paste(auc_cort_bc$condition, auc_cort_bc$time, sep = "_")

# Delete condition and time columns
auc_cort_bc <- auc_cort_bc[,-c(2,3)]

# List of subject numbers with complete cases used above
use_subj <- auc_cort_wide$subjNum

# Subset only those subjects
auc_cort_bc <- auc_cort_bc %>%
  filter(subjNum %in% use_subj)

auc_cort_bc_wide <- auc_cort_bc %>%
  pivot_wider(names_from = condition_time, values_from = mean_cort)

# Initialize an empty vector to store the AUC values
auc_values_ctrl_bc <- numeric(nrow(auc_cort_bc_wide))
auc_values_cp_bc <- numeric(nrow(auc_cort_bc_wide))
auc_values_fire_bc <- numeric(nrow(auc_cort_bc_wide))

# Loop through each participant and condition to calculate the AUC
for (i in 1:nrow(auc_cort_bc_wide)) {
  # Get the x and y values for the current participant and condition
  x <- c(1, 2, 3)
  y <- c(auc_cort_bc_wide$ctrl_bc_post1[i], auc_cort_bc_wide$ctrl_bc_post15[i], auc_cort_bc_wide$ctrl_bc_post30[i])
  y1 <- c(auc_cort_bc_wide$cp_bc_post1[i], auc_cort_bc_wide$cp_bc_post15[i], auc_cort_bc_wide$cp_bc_post30[i])
  y2 <- c(auc_cort_bc_wide$fire_bc_post1[i], auc_cort_bc_wide$fire_bc_post15[i], auc_cort_bc_wide$fire_bc_post30[i])
  
  # Calculate AUC using the trapz function
  auc_values_ctrl_bc[i] <- trapz(x, y)
  auc_values_cp_bc[i] <- trapz(x,y1)
  auc_values_fire_bc[i] <- trapz(x,y2)
}

# Create a new data frame with participant IDs, conditions, and their respective AUC values
auc_table_bc <- data.frame(
  subjNum = auc_cort_bc_wide$subjNum,
  ctrl = auc_values_ctrl_bc, 
  cp = auc_values_cp_bc,
  fire = auc_values_fire_bc
)

# Make table long
auc_table_long_bc <- auc_table_bc %>%
  pivot_longer(!subjNum, names_to = "condition", values_to = "auc")

# Anova
hist(auc_table_long_bc$auc)
hist(log(auc_table_long_bc$auc))
auc_table_long_bc$auc_log <- log(auc_table_long_bc$auc+1)

res.aov_bc <- aov(auc ~ condition, data = auc_table_long_bc)
summary(res.aov_bc) 

# posthoc tests
TukeyHSD(res.aov_bc)

# Graph
ggplot(auc_table_long_bc, aes(x = condition, y = auc_log)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75))

# Make table for each complete subjects auc and auc_bc

# Rename the bc table columns
auc_table_bc <- auc_table_bc %>%
  rename_with(~paste0(.,"_bc"), c(ctrl, cp, fire))

auc_table_complete_cases <- merge(auc_table, auc_table_bc, by = "subjNum")

# Write combined_data to a new csv file
# write.csv(auc_table_complete_cases, paste0("E:/Nav Stress Data/auc_table.csv"), row.names = FALSE)

##### Gender split

# Plot actual cortisol amount with all participant separated by condition, time, gender

summary_table <- all_data %>%
  group_by(condition, time, gender) %>%
  summarize(
    count = n(),
    mean_cort_log = mean(log_cort), 
    sd_cort_log = sd(log_cort)
  )

# Make a plot

gender_labels <- c("f" = "Women", "m" = "Men")
condition_labels <- c("cp" = "Cold Pressor", "ctrl" = "Control", "fire" = "Fire Environment")

cort_graph2 <- ggplot(data = summary_table, aes(x=factor(time, level = level_order), y = mean_cort_log, color = condition, group = condition)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~gender, labeller = labeller(gender = gender_labels)) +
  labs(x = "Time", y = " Log Cortisol (nmol/L)", color = "condition") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  scale_x_discrete(labels = c("Pre", "Post1", "Post15", "Post30")) +
  scale_color_manual(name = "Condition", labels = condition_labels, values = c("cp" = "deep sky blue", "ctrl" = "lime green", "fire" = "salmon"))
  
jpeg("E:/Nav Stress Data/dissertation_pics/log_cortisol_gender.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph2
dev.off()

res.aov <- anova_test(data = all_data, dv = log_cort, wid = subjNum, within = c(condition,time), between = gender)
get_anova_table(res.aov) # time and condition:time sig







# graph for iNAV
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsMean <- ggplot(NO_nav_summary, aes(x = grid_type, y = mean_grid_number, fill = condition)) + 
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
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/navTrialsMedian.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
navTrialsMean
#dev.off()
