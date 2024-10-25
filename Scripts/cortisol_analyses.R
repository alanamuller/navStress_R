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
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Line plot with Ss's amounts connected
ggplot(data = whole_cort, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Plot with baseline corrected cort
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Line plot with Ss's amounts connected
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

##### Pics without 10, 15, 20, 23, 28 that didn't finish the cp task

# Participants to exclude
participants_to_exclude <- c(10, 15, 20, 23, 28)

# Exclude data from the specified participants
whole_cort_cpSuccess <- whole_cort %>%
  filter(!subjNum %in% participants_to_exclude)
whole_cort_cpSuccess$log_cort <- log(whole_cort_cpSuccess$mean_cort)

bc_cort_cpSuccess <- bc_cort %>%
  filter(!subjNum %in% participants_to_exclude)
bc_cort_cpSuccess$log_cort <- log(bc_cort_cpSuccess$mean_cort)


# Make a separate dataframe with only complete cases
# Participants to exclude
incompletes_to_exclude <- c(1, 2, 3, 5, 6, 8, 9, 16, 22)

# Exclude data from the specified participants
whole_cort_complete <- whole_cort_cpSuccess %>%
  filter(!subjNum %in% incompletes_to_exclude)

# Exclude data from the specified participants
bc_cort_complete <- bc_cort_cpSuccess %>%
  filter(!subjNum %in% incompletes_to_exclude)
bc_cort_complete$log_cort <- log(bc_cort_complete$mean_cort)


# Plot actual cortisol amount with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Line plot with Ss's amounts connected
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Plot with baseline corrected cort
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Line plot with Ss's amounts connected
ggplot(data = bc_cort, aes(x=factor(time, level = bc_level_order), y=mean_cort)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

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
get_anova_table(res.aov) # condition*time interaction sig

# testing simple main effects

# testing effect of condition at every time point
one.way <- whole_cort_cpSuccess %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way # post15 is sig

# equal cases so pairing it will work
pwc <- whole_cort_complete %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc # cp and fire are sig diff at post15 and post30, nothing else


one.way2 <- whole_cort_cpSuccess %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2 # cp is sig

# equal cases so pairing it will work
pwc2 <- whole_cort_complete %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2 # cp (post15 vs post 30) and (post15 vs pre) are sig


# Make a plot
cond.labs <- c("Cold Pressor", "Control Condition", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol" )

# Make a plot
cond.labs <- c("Cold Pressor", "Control", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
cort_graph1 <- ggplot(data = whole_cort_complete, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
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
# This one doesn't make a lot of sense since the data are still skewed but I can't transform it

# Checking normality
ggqqplot(bc_cort_complete$mean_cort)
hist(bc_cort_complete$mean_cort)
hist(log(bc_cort_complete$mean_cort))
ggqqplot(bc_cort_complete$log_cort)

normality_bc_cort <- bc_cort_complete %>%
  group_by(time) %>%
  shapiro_test(mean_cort)
normality_bc_cort

# anova
res.aov <- anova_test(data = bc_cort_complete, dv = mean_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # nothing sig

# testing simple main effects

# testing effect of condition at every time point
one.way <- bc_cort_complete %>%
  group_by(time) %>%
  anova_test(dv = mean_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# equal cases so pairing it will work
pwc <- bc_cort_complete %>%
  group_by(time) %>%
  pairwise_t_test(
    mean_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


one.way2 <- bc_cort_complete %>%
  group_by(condition) %>%
  anova_test(dv = mean_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# equal cases so pairing it will work
pwc2 <- bc_cort_complete %>%
  group_by(condition) %>%
  pairwise_t_test(
    mean_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


########### AUC stuff

# Little bit of code to verify the method

# calculate area under curve
x <- c(1,2,3)
y <- c(2,5,3)
plot(x,y)
lines(x,y)

auc <- trapz(x,y)

# Example data: 4 pairs of x and y values
x <- c(1, 2, 3, 4)
y <- c(2,2,2,2)

# Calculate area under the curve
auc <- trapz(x, y)
print(paste("AUC:", auc))

# Plot the curve
plot(x, y, type = "b", col = "blue", pch = 16, xlab = "X", ylab = "Y", 
     main = "Trapezoidal Approximation of Area Under the Curve")

# Add trapezoids
for (i in 1:(length(x) - 1)) {
  # Draw the trapezoid by connecting consecutive points and the x-axis
  polygon(c(x[i], x[i], x[i + 1], x[i + 1]), c(0, y[i], y[i + 1], 0), 
          col = rgb(0.2, 0.7, 0.2, 0.4), border = "darkgreen")
}

# Re-draw the curve on top of the trapezoids
lines(x, y, type = "b", col = "blue", pch = 16)


##### Actual real data: Subset raw cort data to put it in wide form
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
write.csv(auc_table_complete_cases, paste0("E:/Nav Stress Data/auc_table.csv"), row.names = FALSE)
