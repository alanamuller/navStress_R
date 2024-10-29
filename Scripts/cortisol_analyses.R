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
wide_data <- pivot_wider(data = all_data, id_cols = c(num_cond, gender), names_from = time, values_from = cort_nmol_L)

# Add baseline corrected columns
wide_data$bc_post1 <- wide_data$post1 - wide_data$pre
wide_data$bc_post15 <- wide_data$post15 - wide_data$pre
wide_data$bc_post30 <- wide_data$post30 - wide_data$pre

# Write this cort data to a table - it could be useful later
mean_cort_table <- wide_data %>%
  separate(num_cond, into = c("subjNum", "condition"), sep = "_")

# Write table to csv
#write.csv(mean_cort_table, paste0("E:/Nav Stress Data/mean_cort_table_nmol_L.csv"), row.names = FALSE)

# Make a long version of the data
long_data <- pivot_longer(data = wide_data, cols = !c(num_cond,gender), names_to = "time", values_to = "mean_cort")

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

#jpeg("E:/Nav Stress Data/dissertation/pics/log_cortisol_allData.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph1
#dev.off()

##### Take out the people that didn't finish the cold pressor task

# Participants to exclude
bad_cp <- c(10, 15, 18, 20, 23, 28)

good_cp_data <- all_data %>%
  filter(!subjNum %in% bad_cp)

# Plot and ANOVA

# anova
res.aov <- anova_test(data = good_cp_data, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # condition and condition*time interaction sig

# testing simple main effects

# testing effect of condition at every time point
one.way_complete <- good_cp_data %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_complete # post15 is sig - same as above

# equal cases so pairing it will work
pwc_complete <- good_cp_data %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_complete # cp and fire are sig diff at post15 and post30, and cp and ctrl at post15 are sig diff - changed from above

one.way2_complete <- good_cp_data %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2_complete # cp is sig - same as above

# equal cases so pairing it will work
pwc2_complete <- good_cp_data %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2_complete # cp, (post1 vs post15), (post15 vs post 30), (post15 vs pre) are sig - changed from above

# Plot with all participant separated by condition and time
cort_graph_complete <- ggplot(data = good_cp_data, aes(x=factor(time, level = level_order), y=log_cort)) +
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

#jpeg("E:/Nav Stress Data/dissertation/pics/log_cortisol_complete.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph_complete
#dev.off()

# Plot the 6 people who did not do the cp task correctly
# They actually all look like they have a cortisol increase for the cp task

# Participants to exclude
bad_cp <- c(10, 15, 18, 20, 23, 28)

bad_cp_data <- all_data %>%
  filter(subjNum %in% bad_cp)

bad_cp_data <- all_data %>%
  filter(subjNum == 28)

# Plot with all participant separated by condition and time
cort_graph_complete <- ggplot(data = bad_cp_data, aes(x=factor(time, level = level_order), y=log_cort)) +
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

#jpeg("E:/Nav Stress Data/dissertation/pics/log_cortisol_complete.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph_complete
#dev.off()

########## Now split by gender

summary_table <- good_cp_data %>%
  group_by(condition, time, gender) %>%
  summarize(
    count = n(),
    mean_cort_log = mean(log_cort), 
    sd_cort_log = sd(log_cort)
  )

# Plot
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

#jpeg("E:/Nav Stress Data/dissertation/pics/log_cortisol_gender.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph2
#dev.off()

# Repeated 3-way ANOVA
res.aov <- anova_test(data = good_cp_data, dv = log_cort, wid = subjNum, within = c(condition,time), between = gender)
get_anova_table(res.aov) # condition and condition:time sig

# testing effect of condition at every time point
one.way_complete <- good_cp_data %>%
  group_by(gender) %>%
  anova_test(dv = log_cort, wid = subjNum, within = c(condition, time)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_complete # post15 condition is sig - same as above

# equal cases so pairing it will work
pwc_complete <- good_cp_data %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_complete # cp and fire are sig diff at post15 and post30, and cp and ctrl at post15 are sig diff - changed from above

one.way2_complete <- good_cp_data %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2_complete # cp is sig - same as above

# equal cases so pairing it will work
pwc2_complete <- good_cp_data %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2_complete # cp, (post1 vs post15), (post15 vs post 30), (post15 vs pre) are sig - changed from above


########### AUC stuff

##### Subset raw cort data to put it in wide form
auc_cort <- whole_cort %>%
  select(subjNum, gender, condition, time, mean_cort)
auc_cort$condition_time <- paste(auc_cort$condition, auc_cort$time, sep = "_")
auc_cort <- subset(auc_cort, select = -c(condition, time)) # delete columns condition and time

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

# Create a new data frame with participant IDs, conditions, gender, and their respective AUC values
auc_table <- data.frame(
  subjNum = auc_cort_wide$subjNum,
  gender = auc_cort_wide$gender,
  ctrl = auc_values_ctrl, 
  cp = auc_values_cp,
  fire = auc_values_fire
)

# Get rid of people who didn't complete cp task
auc_table <- auc_table %>%
  filter(!subjNum %in% bad_cp)


# Make table long
auc_table_long <- auc_table %>%
  pivot_longer(!c(subjNum, gender), names_to = "condition", values_to = "auc")

# One way ANOVA
hist(auc_table_long$auc)
hist(log(auc_table_long$auc))
auc_table_long$auc_log <- log(auc_table_long$auc)

res.one_way <- anova_test(data = auc_table_long, dv = auc_log, wid = subjNum, within = condition)
get_anova_table(res.one_way) # condition is sig

# Post-hoc tests
pwc_auc <- auc_table_long %>%
  pairwise_t_test(
    auc_log ~ condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_auc # cp and fire sig diff


# Graph
ggplot(auc_table_long, aes(x = condition, y = auc_log)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(color = "gray60") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  geom_line(aes(group = subjNum), color = "gray80", alpha = 0.7)

# AUC Gender split - is AUC diff by gender?

# ANOVA
res.aov <- anova_test(data = auc_table_long, dv = auc_log, wid = subjNum, within = condition, between = gender)
get_anova_table(res.aov) # only condition sig

# Graph with gender
AUC_graph <- ggplot(auc_table_long, aes(x = condition, y = auc_log, fill = gender)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "Condition", y = " Log AUC") +  
  theme_classic() +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  scale_x_discrete(labels = c("SECPT", "Control", "Fire")) +
  scale_fill_manual(name = "Gender", labels = c("Women", "Men"), values = c("sienna1", "steelblue2"))

#jpeg("E:/Nav Stress Data/dissertation/pics/AUC_gender.jpeg", width = 7, height = 5.75, units = 'in', res = 500)
AUC_graph
#dev.off()

# Write table to csv
#write.csv(auc_table_complete_cases, paste0("E:/Nav Stress Data/auc_table.csv"), row.names = FALSE)

##### Now do the same for the baseline corrected data

##### Actual real data: Subset baseline corrected cort data to put it in wide form
auc_cort_bc <- bc_cort %>%
select(subjNum, gender, condition, time, mean_cort)

# Join condition and time column together
auc_cort_bc$condition_time <- paste(auc_cort_bc$condition, auc_cort_bc$time, sep = "_")
auc_cort_bc <- subset(auc_cort_bc, select = -c(condition, time)) # delete columns condition and time

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
pivot_longer(!subjNum, names_to = "condition", values_to = "auc_bc")

# Write table to csv
write.csv(auc_table_long_bc, paste0("E:/Nav Stress Data/auc_bc_table.csv"), row.names = FALSE)
