library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(magrittr)
library(dplyr)
library(pROC)

# Start fresh by removing everything from the environment
rm(list = ls())

# Set working directory
#setwd("D:/Nav Stress Data/Salimetrics reports") # for laptop
setwd("E:/Nav Stress Data/Salimetrics reports") # for hard drive

# Enter the data 
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

samples9Data$cort_nmol_L <- samples9Data$mean_cort*276
samples12Data$cort_nmol_L <- samples12Data$mean_cort*276

all_data <- rbind(samples9Data, samples12Data)

small_data <- samples9Data %>% 
  filter(subjNum >= 2 & subjNum <= 3)

data_ctrl <- samples9Data %>%
  filter(condition == "ctrl")

data_cp <- samples9Data %>%
  filter(condition == "cp")

data_fire <- samples9Data %>%
  filter(condition == "fire")

summaryStats <- samples9Data%>%
  group_by(condition, time) %>%
  get_summary_stats(mean_cort, type = "mean_sd")

# Set the order of the x-axis
level_order <- c('pre', 'post1', 'post15', 'post30')


##### Pics of all data

# Plot with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

ggplot(data = all_data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

bxp <- ggboxplot(
  all_data, x = "time", y = "cort_nmol_L", 
  color = "condition"
)
bxp

##### Pics of 9 sample data

# Plot with all participant separated by condition and time
ggplot(data = samples9Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

ggplot(data = samples9Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

##### Pics of 12 sample data

# Plot with all participant separated by condition and time
ggplot(data = samples12Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

ggplot(data = samples12Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

########## Checking assumptions

##### All data

# all_data outliers - 4 outliers for now
outliers_allData <- all_data %>%
  group_by(time, condition) %>%
  identify_outliers(cort_nmol_L)

mean_cort <- mean(all_data$cort_nmol_L)
sd_cort <- sd(all_data$cort_nmol_L)

no_outliers_allData <- subset(all_data, all_data$cort_nmol_L > mean_cort - sd_cort*2.5 & all_data$cort_nmol_L < mean_cort + sd_cort*2.5)

no_outliers_allData$log_cort <- log(no_outliers_allData$cort_nmol_L)

# Checking normality
ggqqplot(no_outliers_allData$cort_nmol_L)
hist(no_outliers_allData$cort_nmol_L)
hist(log(no_outliers_allData$cort_nmol_L))

normality_allData <- no_outliers_allData %>%
  group_by(time) %>%
  shapiro_test(log_cort)

# quick and dirty anova
res.aov <- anova_test(data = no_outliers_allData, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov)

# testing main effects since the interaction wasn't sig

# Make a plot
cond.labs <- c("Cold Pressor", "Control Condition", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
ggplot(data = no_outliers_allData, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol (log nmol/L)" )

########### trying AUC stuff

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





# Example data structure (replace this with your actual data)
# Assume 'participant_id' identifies each participant
# 'x1', 'x2', 'x3', 'x4' are the x-values, and 'y1', 'y2', 'y3', 'y4' are the y-values
data <- data.frame(
  participant_id = 1:33,
  x1 = runif(33), x2 = runif(33), x3 = runif(33), x4 = runif(33),
  y1 = runif(33), y2 = runif(33), y3 = runif(33), y4 = runif(33)
)

data$x1 <- 1
data$x2 <- 2
data$x3 <- 3
data$x4 <- 4

data[,6:9] <- 10



# Initialize an empty vector to store the AUC values
auc_values <- numeric(nrow(data))

# Loop through each participant to calculate the AUC
for (i in 1:nrow(data)) {
  # Get the x and y values for the current participant
  x <- c(data$x1[i], data$x2[i], data$x3[i], data$x4[i])
  y <- c(data$y1[i], data$y2[i], data$y3[i], data$y4[i])
  
  # Calculate AUC using the trapz function
  auc_values[i] <- trapz(x, y)
  plot(x,y)
}

# Create a new data frame with participant IDs and their respective AUC values
auc_table <- data.frame(
  participant_id = data$participant_id,
  auc = auc_values
)

# View the AUC table
print(auc_table)


