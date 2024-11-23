library(stringr)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(ggplot2)

# Made by Alana Muller with a lot of help from ChatGPT

rm(list = ls())

# Store coordinates

# Store 2 - (X,Z): 207.3, 99.9
# Store 3 - (X,Z): 145.79, -231.68
# Store 4 - (X,Z): -130.43, -112.92
# Store 1 - (X,Z): -249.37, 279.16

#points(-249.37, 279.16)
#points(207.3, 99.9)
#points(145.79, -231.68)
#points(-130.43, -112.92)

# run for pilot Ss: 1, 2, 3, 4, 13, 14, 15, 16, 17, 18, 19, 20, 22

# Set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")
setwd("E:/Nav Stress Data/pilot") # for hard drive
# setwd("C:/Users/almul/OneDrive/Desktop/Alana/UA/HSCL/Stress Shortcuts")

##### Change this to run next subject

subject_num <- "P001"

# Load the data
input_file <- paste(subject_num, ".log", sep = "")
# input_file <- "mini_test_log.txt"
# input_file <- "environment_corner_coordinates.log"
input_data <- paste(readLines(input_file), collapse="\n")
text <- input_data

# set working directory to save pics - make sure a new folder is created already for the subject's pics
folder_name <- paste("E:/Nav Stress Data/pilot/pics/", subject_num, sep = "")

setwd(folder_name)

###################### Functions ######################

# function to calculate total distance of the path - aka totDist
totDist <- function(x,y) {
  sum(sqrt(diff(x)^2 + diff(y)^2))
}

##################################### EXTRACT OUTER PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeOuterPaths and TASK_END TASK_EncodeOuterPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeOuterPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeOuterPaths") # finds the data between the start and end point
outer_df <- data.frame(matches, stringsAsFactors = FALSE) # make one big dataframe for outer path
colnames(outer_df)[1] <- "learn_outer_paths"

############# Extract all lines between TASK_START LearnSmoothPassive SmoothPassivePathStart and TASK_END LearnSmoothPassive SmoothPassivePathStart

matches <- str_extract_all(outer_df[1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+SmoothPassivePathStart.*?TASK_END\\s+LearnSmoothPassive\\s+SmoothPassivePathStart")
outer_passive_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(outer_passive_df)[1] <- "outer_passive_task"

outer_passive_df_list <- lapply(seq_len(nrow(outer_passive_df)), function(i) data.frame(value = outer_passive_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(outer_passive_df_list)) {
  
  # Get the dataframe from the list
  data_df <- outer_passive_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("outer_passive", i)
  
  # Update the dataframe in the list
  outer_passive_df_list[[i]] <- data_df
  
}

# make and save a graph
p <- ggplot(outer_passive_df_list[[1]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "Outer Path Passive Learning 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg("outer_passive1.jpeg", width = 7, height = 6, units = 'in', res = 500)
p
#dev.off()

############# Extract all lines between TASK_START LearnActivePath ActivePathStart and TASK_END LearnActivePath ActivePathStart

matches <- str_extract_all(outer_df[1], "(?s)TASK_START\\s+LearnActivePath\\s+ActivePathStart.*?TASK_END\\s+LearnActivePath\\s+ActivePathStart")
outer_active_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(outer_active_df)[1] <- "outer_active_task"

outer_active_df_list <- lapply(seq_len(nrow(outer_active_df)), function(i) data.frame(value = outer_active_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(outer_active_df_list)) {
  
  # Get the dataframe from the list
  data_df <- outer_active_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("outer_active", i)
  
  # Update the dataframe in the list
  outer_active_df_list[[i]] <- data_df
  
}

# make and save graph of last outer active
plot_name <- paste("outer_active", length(outer_active_df_list), ".jpg",sep = "")
p <- ggplot(outer_active_df_list[[length(outer_active_df_list)]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "Outer Path Active Learning 4") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg(plot_name, width = 7, height = 6, units = 'in', res = 500)
p
#dev.off()

# Use last trial as the actual whole path length
outer_actual_dist <- totDist(outer_active_df_list[[length(outer_active_df_list)]]$pos_X, outer_active_df_list[[length(outer_active_df_list)]]$pos_Z)


############# Extract all lines between TASK_START Navigate	NavigationTask and TASK_END Navigate NavigationTask	

matches <- str_extract_all(outer_df[1], "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
outer_navInOrder_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(outer_navInOrder_df)[1] <- "outer_navInOrder_task"

outer_navInOrder_df_list <- lapply(seq_len(nrow(outer_navInOrder_df)), function(i) data.frame(value = outer_navInOrder_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(outer_navInOrder_df_list)) {
  
  # Get the dataframe from the list
  data_df <- outer_navInOrder_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("outer_navInOrder", i)
  
  # Update the dataframe in the list
  outer_navInOrder_df_list[[i]] <- data_df
  
}

# make one big dataframe with all outer nav in order x z values
outer_navInOrder_all_dfs <- do.call(rbind, outer_navInOrder_df_list)

# this is the participant's whole traveled path (segments combined together)
#x <- outer_navInOrder_all_dfs$pos_X
#z <- outer_navInOrder_all_dfs$pos_Z
#plot(x,z)

p <- ggplot(outer_navInOrder_all_dfs, aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "Recreated Outer Path") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
  scale_y_continuous(breaks = seq(-400,400,100)) +
  scale_x_continuous(breaks = seq(-400,400,100)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -300, y = 350, label = "Store 1"), size = 7, color = "black") +
  geom_text(aes(x = 280, y = 140, label = "Store 2"), size = 7, color = "black") +
  geom_text(aes(x = 220, y = -275, label = "Store 3"), size = 7, color = "black") +
  geom_text(aes(x = -230, y = -130, label = "Store 4"), size = 7, color = "black")

#jpeg("outer_navInOrder.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500)
p
#dev.off()

##################################### EXTRACT INNER PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeInnerPaths and TASK_END TASK_EncodeInnerPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeInnerPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeInnerPaths") # finds the data between the start and end point
inner_df <- data.frame(matches, stringsAsFactors = FALSE) # make one big dataframe for the inner path
colnames(inner_df)[1] <- "learn_inner_paths"

############# Extract all lines between TASK_START LearnSmoothPassive SmoothPassivePathStart and TASK_END LearnSmoothPassive SmoothPassivePathStart

matches <- str_extract_all(inner_df[1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+SmoothPassivePathStart.*?TASK_END\\s+LearnSmoothPassive\\s+SmoothPassivePathStart")
inner_passive_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(inner_passive_df)[1] <- "inner_passive_task"

inner_passive_df_list <- lapply(seq_len(nrow(inner_passive_df)), function(i) data.frame(value = inner_passive_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(inner_passive_df_list)) {
  
  # Get the dataframe from the list
  data_df <- inner_passive_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. inner_passive1)
  data_df$trialname <- paste0("inner_passive", i)
  
  # Update the dataframe in the list
  inner_passive_df_list[[i]] <- data_df
  
}

p <- ggplot(inner_passive_df_list[[1]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "Inner Path Passive Learning 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg("inner_passive1.jpeg", width = 7, height = 6, units = 'in', res = 500)
p
#dev.off()

############# Extract all lines between TASK_START LearnActivePath ActivePathStart and TASK_END LearnActivePath ActivePathStart

matches <- str_extract_all(inner_df[1], "(?s)TASK_START\\s+LearnActivePath\\s+ActivePathStart.*?TASK_END\\s+LearnActivePath\\s+ActivePathStart")
inner_active_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(inner_active_df)[1] <- "inner_active_task"

inner_active_df_list <- lapply(seq_len(nrow(inner_active_df)), function(i) data.frame(value = inner_active_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(inner_active_df_list)) {
  
  # Get the dataframe from the list
  data_df <- inner_active_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. inner_passive1)
  data_df$trialname <- paste0("inner_active", i)
  
  # Update the dataframe in the list
  inner_active_df_list[[i]] <- data_df
  
}

plot_name <- paste("inner_active", length(inner_active_df_list), ".jpg", sep = "")
p <- ggplot(inner_active_df_list[[length(inner_active_df_list)]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "Inner Path Active Learning 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg(plot_name, width = 7, height = 6, units = 'in', res = 500)
p
#dev.off()

# inner path actual path length
inner_actual_dist <- totDist(inner_active_df_list[[length(inner_active_df_list)]]$pos_X, inner_active_df_list[[length(inner_active_df_list)]]$pos_Z)

############# Extract all lines between TASK_START Navigate	NavigationTask and TASK_END Navigate NavigationTask	

matches <- str_extract_all(inner_df[1], "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
inner_navInOrder_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(inner_navInOrder_df)[1] <- "inner_navInOrder_task"

inner_navInOrder_df_list <- lapply(seq_len(nrow(inner_navInOrder_df)), function(i) data.frame(value = inner_navInOrder_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(inner_navInOrder_df_list)) {
  
  # Get the dataframe from the list
  data_df <- inner_navInOrder_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. inner_passive1)
  data_df$trialname <- paste0("inner_navInOrder", i)
  
  # Update the dataframe in the list
  inner_navInOrder_df_list[[i]] <- data_df
  
}

# make one big dataframe with all inner nav in order x z values
inner_navInOrder_all_dfs <- do.call(rbind, inner_navInOrder_df_list)

# participant's whole path
#x <- inner_navInOrder_all_dfs$pos_X
#z <- inner_navInOrder_all_dfs$pos_Z
#plot(x,z)


p <- ggplot(inner_navInOrder_all_dfs, aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "Recreate Inner Path") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
  scale_y_continuous(breaks = seq(-400,400,100)) +
  scale_x_continuous(breaks = seq(-400,400,100)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -300, y = 350, label = "Store 1"), size = 7, color = "black") +
  geom_text(aes(x = 280, y = 140, label = "Store 2"), size = 7, color = "black") +
  geom_text(aes(x = 220, y = -275, label = "Store 3"), size = 7, color = "black") +
  geom_text(aes(x = -230, y = -130, label = "Store 4"), size = 7, color = "black")

#jpeg("Inner_navInOrder.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500)
p
#dev.off()

##################### Getting the closest points to separate the whole active path into four segments #####################

# Define the points to search for (these are the store coordinates)
search_points <- data.frame(x = c(207.3, 145.79, -130.43, -249.37), y = c(99.9, -231.68, -112.92, 279.16))

########### OUTER PATH ###########

# Find the closest point to each search point
outer_closest_points <- lapply(1:nrow(search_points), function(i) {
  
  # Calculate distances to all points in data frame
  distances <- sqrt((outer_active_df_list[[length(outer_active_df_list)]]$pos_X - search_points[i, "x"])^2 + (outer_active_df_list[[length(outer_active_df_list)]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = outer_active_df_list[[length(outer_active_df_list)]]$pos_X[min_index], 
                    y = outer_active_df_list[[length(outer_active_df_list)]]$pos_Z[min_index], 
                    index = min_index,
                    time = outer_active_df_list[[length(outer_active_df_list)]]$time_sec[min_index]))
})

# Combine the closest points into a single data frame
outer_closest_points_df <- do.call(rbind, outer_closest_points)

# Print the closest points
print(outer_closest_points_df)

### Make a list of dataframes to segment one path into four paths

# Initialize the dataframe
outer_active_seg_list <- list()

# Store 1 to store 2
outer_active_seg_list[[1]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec <= outer_closest_points_df$time[1])
# Store 2 to store 3
outer_active_seg_list[[2]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[1] & time_sec <= outer_closest_points_df$time[2])
# Store 3 to store 4
outer_active_seg_list[[3]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[2] & time_sec <= outer_closest_points_df$time[3])
# Store 4 to store 1
outer_active_seg_list[[4]] <- outer_active_df_list[[length(outer_active_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[3])

# Make an empty dataframe to put the path segment distance values into
outer_actual_seg_dist <- data.frame(segment_distance = numeric(), stringsAsFactors = FALSE)

# Fill dataframe with actual path distance values for each segment
for (i in seq_along(outer_active_seg_list)) {
  actual_seg_dist <- totDist(outer_active_seg_list[[i]]$pos_X, outer_active_seg_list[[i]]$pos_Z)
  
  # put info in dataframe
  outer_actual_seg_dist <- rbind(outer_actual_seg_dist, data.frame(segment_distance = actual_seg_dist))
  
}

########### INNER PATH ###########

# Find the closest point to each search point
inner_closest_points <- lapply(1:nrow(search_points), function(i) {
  
  # Calculate distances to all points in data frame
  distances <- sqrt((inner_active_df_list[[length(inner_active_df_list)]]$pos_X - search_points[i, "x"])^2 + (inner_active_df_list[[length(inner_active_df_list)]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = inner_active_df_list[[length(inner_active_df_list)]]$pos_X[min_index], 
                    y = inner_active_df_list[[length(inner_active_df_list)]]$pos_Z[min_index], 
                    index = min_index,
                    time = inner_active_df_list[[length(inner_active_df_list)]]$time_sec[min_index]))
})

# Combine the closest points into a single data frame
inner_closest_points_df <- do.call(rbind, inner_closest_points)

# Print the closest points
print(inner_closest_points_df)

### Make a list of dataframes to segment one path into four paths

# Initialize the dataframe
inner_active_seg_list <- list()

# Store 1 to store 2
inner_active_seg_list[[1]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec <= inner_closest_points_df$time[1])
# Store 2 to store 3
inner_active_seg_list[[2]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[1] & time_sec <= inner_closest_points_df$time[2])
# Store 3 to store 4
inner_active_seg_list[[3]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[2] & time_sec <= inner_closest_points_df$time[3])
# Store 4 to store 1
inner_active_seg_list[[4]] <- inner_active_df_list[[length(inner_active_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[3])

# Make an empty dataframe to put the path segment distance values into
inner_actual_seg_dist <- data.frame(segment_distance = numeric(), stringsAsFactors = FALSE)

# Fill dataframe with actual path distance values for each segment
for (i in seq_along(inner_active_seg_list)) {
  actual_seg_dist <- totDist(inner_active_seg_list[[i]]$pos_X, inner_active_seg_list[[i]]$pos_Z)
  
  # put info in dataframe
  inner_actual_seg_dist <- rbind(inner_actual_seg_dist, data.frame(segment_distance = actual_seg_dist))
  
}

##################################### RETRIEVE: NAVIGATION TASK #####################################

############# Extract all lines between TASK_START TASK_NavigationTest and TASK_END TASK_NavigationTest

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_NavigationTest\\s+(.*?)\\s+TASK_END\\s+TASK_NavigationTest") # finds the data between the start and end point
navTest_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunk of data 
colnames(navTest_df)[1] <- "navInOrder_task" # renames the first column

############# Extract all lines between TASK_START	Navigate	NavigationTask and TASK_END	Navigate	NavigationTask

matches <- str_extract_all(navTest_df[1], "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
navTest_trials_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(navTest_trials_df)[1] <- "navTest_trials_task"

navTest_trials_df_list <- lapply(seq_len(nrow(navTest_trials_df)), function(i) data.frame(value = navTest_trials_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(navTest_trials_df_list)) {
  
  # Get the dataframe from the list
  data_df <- navTest_trials_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")
  
  # Convert the columns to numeric
  data_df <- data_df %>%
    mutate(across(2:8, as.numeric))
  
  # Remove the rows with NAs
  data_df <- na.omit(data_df)
  
  # Make column for time stamp by making avatar number start from 0
  avatar_initial_number <- data_df$avatar_number[1]
  data_df <- data_df %>%
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("navTest_trials", i)
  
  # Update the dataframe in the list
  navTest_trials_df_list[[i]] <- data_df
  
}

# make one big dataframe with all nav trials' x z values
navTest_all_dfs <- do.call(rbind, navTest_trials_df_list)

p <- ggplot(navTest_all_dfs, aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time (s)", title = "All Navigation Test Trials") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
  scale_y_continuous(breaks = seq(-400,400,100)) +
  scale_x_continuous(breaks = seq(-400,400,100)) +
  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  geom_text(aes(x = -300, y = 350, label = "Store 1"), size = 7, color = "black") +
  geom_text(aes(x = 280, y = 140, label = "Store 2"), size = 7, color = "black") +
  geom_text(aes(x = 220, y = -275, label = "Store 3"), size = 7, color = "black") +
  geom_text(aes(x = -230, y = -130, label = "Store 4"), size = 7, color = "black")

#jpeg("all_navTest_trials.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500)
p
#dev.off()

############# Make another dataframe pulling the numbers that Mike generated in the avatar log (has optimal path)
############# This data frame combines with the overlapping segment code at the bottom

# Get the dataframe from the list
log_data <- input_data

# Convert the data to a tibble
log_data <- tibble(log_data = str_split(log_data, "\n")[[1]])

# Extract the line after "task block trial"
log_data <- log_data %>%
  mutate(line = str_extract(log_data, "^TASK_NavigationTest.*$"))

# Split the line into separate columns
log_data <- log_data %>%
  separate(line, into = c("task", "block", "trial", "catchTrial", "Navigate_target", "Navigate_actualPath", "Navigate_optimalPath", "Navigate_excessPath", "Navigate_duration"), sep = "\\t+")

# Remove the rows with NAs
log_data <- na.omit(log_data)

# Convert the columns to numeric
log_data <- log_data %>%
  mutate(across(7:10, as.numeric))

log_data <- log_data[, -1]

# Add a column for subject ID
log_data$subjectID <- subject_num

# Make the subjectID column the first column
log_data <- log_data[c(ncol(log_data), 1:ncol(log_data)-1)]

####################### Make another dataframe with learning trials: total path length, excess path length, and duration values
# But this total path distance is comparing the learned path to the traveled path

# Make an empty dataframe to put the path distance values into
path_dist_df <- data.frame(trialname = character(), total_path_distance = numeric(), excess_path_distance = numeric(), path_duration = numeric(), stringsAsFactors = FALSE)

########## OUTER PASSIVE ##########
for (i in 1:length(outer_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_passive_df_list[[i]]$pos_X, outer_passive_df_list[[i]]$pos_Z)
  
  # no excess path distance to calculate here
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = outer_passive_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = "N/A", path_duration = path_dur))
  
}

########## OUTER ACTIVE ##########
for (i in 1:length(outer_active_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_active_df_list[[i]]$pos_X, outer_active_df_list[[i]]$pos_Z)
  
  # calculate excess path distance
  excess_path = path_dist - outer_actual_dist
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_active_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = outer_active_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

########## OUTER NAV IN ORDER WHOLE PATH ##########
path_dist <- totDist(outer_navInOrder_all_dfs$pos_X, outer_navInOrder_all_dfs$pos_Z)
excess_path <- path_dist - outer_actual_dist
path_dur <- sum(max(outer_navInOrder_df_list[[1]]$time_sec), max(outer_navInOrder_df_list[[2]]$time_sec),
                max(outer_navInOrder_df_list[[3]]$time_sec), max(outer_navInOrder_df_list[[4]]$time_sec))

path_dist_df <- rbind(path_dist_df, data.frame(trialname = "outer_navInOrder_all", total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))

########## OUTER NAV IN ORDER SEGMENTS ##########
for (i in 1:length(outer_navInOrder_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_navInOrder_df_list[[i]]$pos_X, outer_navInOrder_df_list[[i]]$pos_Z)
  
  # calculate excess path distances
  excess_path = path_dist - outer_actual_seg_dist[i, 1]
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_navInOrder_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = outer_navInOrder_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

########## INNER PASSIVE ##########
for (i in 1:length(inner_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_passive_df_list[[i]]$pos_X, inner_passive_df_list[[i]]$pos_Z)
  
  # no excess path distance to calculate here
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = inner_passive_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = "N/A", path_duration = path_dur))
  
}

########## INNER ACTIVE ##########
for (i in 1:length(inner_active_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_active_df_list[[i]]$pos_X, inner_active_df_list[[i]]$pos_Z)
  
  # calculate excess path distance
  excess_path = path_dist - inner_actual_dist
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_active_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = inner_active_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

########## INNER NAV IN ORDER WHOLE PATH ##########
path_dist <- totDist(inner_navInOrder_all_dfs$pos_X, inner_navInOrder_all_dfs$pos_Z)
excess_path <- path_dist - inner_actual_dist
path_dur <- sum(max(inner_navInOrder_df_list[[1]]$time_sec), max(inner_navInOrder_df_list[[2]]$time_sec),
                max(inner_navInOrder_df_list[[3]]$time_sec), max(inner_navInOrder_df_list[[4]]$time_sec))

path_dist_df <- rbind(path_dist_df, data.frame(trialname = "inner_navInOrder_all", total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))

########## INNER NAV IN ORDER SEGMENTS ##########
for (i in 1:length(inner_navInOrder_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_navInOrder_df_list[[i]]$pos_X, inner_navInOrder_df_list[[i]]$pos_Z)
  
  # calculate excess path distances
  excess_path = path_dist - inner_actual_seg_dist[i, 1]
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_navInOrder_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = inner_navInOrder_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

# Add a column for subject ID
path_dist_df$subjectID <- subject_num

# Make the subjectID column the first column
path_dist_df <- path_dist_df[c(ncol(path_dist_df), 1:ncol(path_dist_df)-1)]

# write dataframe to an excel file

file_name <- paste(subject_num, "_partial_data.xlsx", sep = "")
#write.xlsx(path_dist_df, file_name, rowNames = FALSE)

####################### Make 24 plots for each nav test trial #######################

# loop through a dataframe list to generate a plot for each trial
#for (i in seq_along(navTest_trials_df_list)) {
  # create the plot title name
#  plot_title <- paste("Navigation Test Trial ", i)
  # create the ggplot object for the current data frame
#  gg <- ggplot(navTest_trials_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#    geom_point() +
#    scale_color_gradient(low = "lightblue", high = "darkblue") +
#    labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
#    theme(plot.title = element_text(hjust = 0.5, size = 20), 
#          axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#          legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#    coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
#    scale_y_continuous(breaks = seq(-400,400,100)) +
#    scale_x_continuous(breaks = seq(-400,400,100)) +
#    geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
#    geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
#    geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
#    geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
#    geom_text(aes(x = -300, y = 350, label = "Store 1"), size = 7, color = "black") +
#    geom_text(aes(x = 280, y = 140, label = "Store 2"), size = 7, color = "black") +
#    geom_text(aes(x = 220, y = -275, label = "Store 3"), size = 7, color = "black") +
#    geom_text(aes(x = -230, y = -130, label = "Store 4"), size = 7, color = "black")
  
  # save the plot with a file name based on the index of the data frame
#  ggsave(paste0("SFNnavTest_trial", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
#}

######################## Calculate overlapping grids ###########################

library(sp)
library(raster)

# define the extent of the area you want to cover - took from the main experiment
xmin <- -361
xmax <- 379
ymin <- -346
ymax <- 393

# define the number of cells in the x and y directions
ncellx <- 37 # 37
ncelly <- 37 # 37

cellsize <- ((xmax - xmin)/ ncellx)

# create a SpatialPolygons object to represent the area
area_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax)))), ID = "1")))

# create a SpatialGrid object to represent the grid over the area
grid <- GridTopology(c(xmin + cellsize/2, ymin + cellsize/2), c(cellsize, cellsize), c(ncellx, ncelly))
grid_sp <- SpatialGrid(grid)
grid_poly <- as(grid_sp, "SpatialPolygons") # add grid lines, need for making plots

################### For navigation test trials ###################

# outer path actual (from passive learning phase)
outerPath_df <- data.frame(x = outer_passive_df_list[[1]]$pos_X, y = outer_passive_df_list[[1]]$pos_Z)
outerPath_sp <- SpatialPoints(outerPath_df)

# inner path actual (from passive learning phase)
innerPath_df <- data.frame(x = inner_passive_df_list[[1]]$pos_X, y = inner_passive_df_list[[1]]$pos_Z)
innerPath_sp <- SpatialPoints(innerPath_df)

# use the "over()" function to find which grid indices contain the x-y coordinates (all path block indices)
outerPath_grid <- over(outerPath_sp, grid_sp)
innerPath_grid <- over(innerPath_sp, grid_sp)

# find the unique indices for the paths (unique block indices) - use for plots to make them easier to read
unique_outerPath_grids <- unique(outerPath_grid)
unique_innerPath_grids <- unique(innerPath_grid)

# find the segments of the inner and outer path that overlap
inner_outer_overlap <- intersect(unique_outerPath_grids, unique_innerPath_grids)

# take out the overlapping grids from the outer and inner grid numbers (number of nonoverlapping unique blocks) - use for data analyses
unique_outerPath_grids_no_Overlap <- unique_outerPath_grids[!unique_outerPath_grids %in% inner_outer_overlap]
unique_innerPath_grids_no_Overlap <- unique_innerPath_grids[!unique_innerPath_grids %in% inner_outer_overlap]

# length of outer and inner paths - for analyses
total_outer_path_grids <- length(unique_outerPath_grids_no_Overlap)
total_inner_path_grids <- length(unique_innerPath_grids_no_Overlap)

# plot the grid and the x-y coordinates within the area
# Define grid title, outlines, and grid lines
grid_plot_title <- "X,Y Coordinates"
plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE, main = grid_plot_title)
lines(grid_poly, col = "gray", add = TRUE)
plot(outerPath_sp, add = TRUE, col = "orange", cex = .5)
#outer_plot <- recordPlot() # capture the current plot
#jpeg("E:/Nav Stress Data/dissertation/pics/outer_passive_xy_coords.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500) # save the plot
#replayPlot(outer_plot)
#dev.off()

# plot the grid for the x-y coordinates within the area
grid_plot_title <- "X,Y Coordinates"
plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE, main = grid_plot_title)
lines(grid_poly, col = "gray", add = TRUE)
plot(innerPath_sp, add = TRUE, col = "lightblue", cex = .5)
#inner_plot <- recordPlot() # capture the current plot
#jpeg("E:/Nav Stress Data/dissertation/pics/inner_passive_xy_coords.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500) # save the plot
#replayPlot(inner_plot)
#dev.off()

# Count the number of overlapping segments for outer and inner path - should be 9 because of the store intesection areas
num_inner_outer_overlap <- length(inner_outer_overlap)

# Create a dataframe to put the data in
overlap_counts_df <- data.frame(overlap_outer = numeric(), overlap_inner = numeric(), nonoverlapping_grid_num = numeric(), total_grids_trial = numeric(), stringsAsFactors = FALSE)

# Define the group of indices you want to color for making plots
indices_outer_red <- unique_outerPath_grids
indices_inner_red <- unique_innerPath_grids
indices_innerOuterOverlap_gray <- inner_outer_overlap

#  Extract polygons corresponding to the selected indices
outer_red <- grid_poly[indices_outer_red]
inner_red <- grid_poly[indices_inner_red]
innerOuter_overlap_gray <- grid_poly[indices_innerOuterOverlap_gray]

# Define grid title, outlines, and grid lines
grid_plot_title <- "Grids Traveled"
plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE, main = grid_plot_title)
lines(grid_poly, col = "gray", add = TRUE)

# Plot the selected polygons
plot(outer_red, col = "orange", add = TRUE)
plot(inner_red, col = "lightblue", add = TRUE)
plot(innerOuter_overlap_gray, col = "purple", add = TRUE) # this is purple to make the graph easier to read
#combo_plot <- recordPlot() # capture the current plot
#jpeg("E:/Nav Stress Data/dissertation/pics/combo_passive_grids.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500) # save the plot
#replayPlot(combo_plot)
#dev.off()

# do a loop to count the overlaps and put it in a dataframe

for (i in 1:length(navTest_trials_df_list)) {
  
  # traveled path (Navigation Test 24 trials)
  navTestTrial_df <- data.frame(x = navTest_trials_df_list[[i]]$pos_X, y = navTest_trials_df_list[[i]]$pos_Z)
  navTestTrial_sp <- SpatialPoints(navTestTrial_df)
  
  # use the "over()" function to find which grids contain the x-y coordinates
  navTestTrial_grid <- over(navTestTrial_sp, grid_sp)
  
  # find the unique numbers for each of the paths representing the total grids the path uses # use for making plots
  unique_navTestTrial_grids <- unique(navTestTrial_grid)
  
  # remove the segments of the inner and outer path that overlap # this is for data analysis
  no_overlap_unique_navTestTrial_grids <- unique_navTestTrial_grids[!unique_navTestTrial_grids %in% inner_outer_overlap]
  
  # find the total number of grids each path uses (without the overlapping grids of course)
  grid_total_trial <- length(no_overlap_unique_navTestTrial_grids)
  
  # Get the indices of the overlapping grids with outer and inner paths
  overlap_outer_indices <- intersect(outerPath_grid, navTestTrial_grid)
  overlap_inner_indices <- intersect(innerPath_grid, navTestTrial_grid)
  
  # Count the number of overlapping and non-overlapping grids
  num_overlapping_outer <- length(overlap_outer_indices)
  num_overlapping_inner <- length(overlap_inner_indices)
  non_overlapping <- grid_total_trial - (length(overlap_outer_indices) + length(overlap_inner_indices))
  
  # Add data to the dataframe
  overlap_counts_df <- rbind(overlap_counts_df, data.frame(overlap_outer = num_overlapping_outer, overlap_inner = num_overlapping_inner, 
                                                           nonoverlapping_grid_num = non_overlapping, total_grids_trial = grid_total_trial))
}

pilot_data_processed <- cbind(log_data, overlap_counts_df)

# write dataframe to an excel file
file_name <- paste(subject_num, "_navTestTrials_pilot.xlsx", sep = "")
write.xlsx(pilot_data_processed, file_name, rowNames = FALSE)

################### For navInOrder learning trials ###################

# Create a dataframe to put the data in
overlap_navInOrder_df <- data.frame(overlap_outer_correct = numeric(), overlap_outer_incorrect = numeric(), overlap_inner_correct = numeric(), overlap_inner_incorrect = numeric(), 
                                    novel_grids_outer = numeric(), novel_grids_inner = numeric(),
                                    total_outer_navInOrder_grids = numeric(), total_inner_navInOrder_grids = numeric(), actual_outer_path_grids = numeric(), actual_inner_path_grids = numeric(), 
                                    stringsAsFactors = FALSE)

# define outer path
outer_navInOrder <- data.frame(x = outer_navInOrder_all_dfs$pos_X, y = outer_navInOrder_all_dfs$pos_Z)
outer_navInOrder_sp <- SpatialPoints(outer_navInOrder)

# define inner path
inner_navInOrder <- data.frame(x = inner_navInOrder_all_dfs$pos_X, y = inner_navInOrder_all_dfs$pos_Z)
inner_navInOrder_sp <- SpatialPoints(inner_navInOrder)

# use the "over()" function to find which grids contain the x-y coordinates
outer_navInOrder_grid <- over(outer_navInOrder_sp, grid_sp)
inner_navInOrder_grid <- over(inner_navInOrder_sp, grid_sp)

# find the unique numbers for each of the paths representing the total grids the path uses
unique_outer_navInOrder_grids <- unique(outer_navInOrder_grid)
unique_inner_navInOrder_grids <- unique(inner_navInOrder_grid)

# take out the overlapping grids from the outer and inner grid numbers (number of nonoverlapping unique blocks) - use for data analyses
unique_outer_navInOrder_grids_no_Overlap <- unique_outer_navInOrder_grids[!unique_outer_navInOrder_grids %in% inner_outer_overlap]
unique_inner_navInOrder_grids_no_Overlap <- unique_inner_navInOrder_grids[!unique_inner_navInOrder_grids %in% inner_outer_overlap]

# length for total outer and inner navInOrder grids
total_outer_navInOrder_path <- length(unique_outer_navInOrder_grids_no_Overlap)
total_inner_navInOrder_path <- length(unique_inner_navInOrder_grids_no_Overlap)

# Get the indices of the outer path overlap with outer and inner grids
outer_navInOrder_overlap <- intersect(unique_outer_navInOrder_grids, unique_outerPath_grids_no_Overlap)
outer_navInOrder_innerOverlap <- intersect(unique_outer_navInOrder_grids,unique_innerPath_grids_no_Overlap)

# Get the indices of the inner path overlap with outer and inner grids
inner_navInOrder_overlap <- intersect(unique_inner_navInOrder_grids, unique_innerPath_grids_no_Overlap)
inner_navInOrder_outerOverlap <- intersect(unique_inner_navInOrder_grids, unique_outerPath_grids_no_Overlap)

# Count the number of overlapping segments for outer and inner path
num_outer_correct_navInOrder_overlap <- length(outer_navInOrder_overlap)
num_outer_incorrect_navInOrder_overlap <- length(outer_navInOrder_innerOverlap)
num_inner_correct_navInOrder_overlap <- length(inner_navInOrder_overlap)
num_inner_incorrect_navInOrder_overlap <- length(inner_navInOrder_outerOverlap)

# Get the indices for the novel grids
indices_novel_outer <- unique_outer_navInOrder_grids_no_Overlap[!unique_outer_navInOrder_grids_no_Overlap %in% unique_outerPath_grids_no_Overlap]
indices_novel_inner <- unique_inner_navInOrder_grids_no_Overlap[!unique_inner_navInOrder_grids_no_Overlap %in% unique_innerPath_grids_no_Overlap]

# Count the novel grids
novel_grids_outer_navInOrder <- total_outer_navInOrder_path - num_outer_correct_navInOrder_overlap - num_outer_incorrect_navInOrder_overlap
novel_grids_inner_navInOrder <- total_inner_navInOrder_path - num_inner_correct_navInOrder_overlap - num_inner_incorrect_navInOrder_overlap

# Add data to the dataframe
overlap_navInOrder_df <- rbind(overlap_navInOrder_df, data.frame(overlap_outer_correct = num_outer_correct_navInOrder_overlap, overlap_outer_incorrect = num_outer_incorrect_navInOrder_overlap, 
                                                                 overlap_inner_correct = num_inner_correct_navInOrder_overlap, overlap_inner_incorrect = num_inner_incorrect_navInOrder_overlap,
                                                                 novel_grids_outer = novel_grids_outer_navInOrder, novel_grids_inner = novel_grids_inner_navInOrder,
                                                                 total_outer_navInOrder_grids = total_outer_navInOrder_path, total_inner_navInOrder_grids = total_inner_navInOrder_path, 
                                                                 actual_outer_path_grids = total_outer_path_grids, actual_inner_path_grids = total_inner_path_grids))

# Add a column for subject ID
overlap_navInOrder_df$subjectID <- subject_num

# Make the subjectID column the first column
overlap_navInOrder_df <- overlap_navInOrder_df[c(ncol(overlap_navInOrder_df), 1:ncol(overlap_navInOrder_df)-1)]

# write dataframe to an excel file
file_name <- paste(subject_num, "_pilot_LearningTrialsOverlap.xlsx", sep = "")
write.xlsx(overlap_navInOrder_df, file_name, rowNames = FALSE)

# Graphs
# Define the group of indices you want to color for making plots
indices_outer_navInOrder <- outer_navInOrder_overlap
indices_inner_navInOrder <- inner_navInOrder_overlap
indices_novel_outer_navInOrder <- indices_novel_outer

#  Extract polygons corresponding to the selected indices
outer_navInOrder_polygons <- grid_poly[indices_outer_navInOrder]
inner_navInOrder_polygons <- grid_poly[indices_inner_navInOrder]
novel_outer_polygons <- grid_poly[indices_novel_outer_navInOrder]

# Define grid title, outlines, and grid lines
grid_plot_title <- "Inner and Outer Recreated"
plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE, main = grid_plot_title)
lines(grid_poly, col = "gray", add = TRUE)

# Plot the selected polygons
plot(outer_navInOrder_polygons, col = "orange", add = TRUE)
plot(inner_navInOrder_polygons, col = "lightblue", add = TRUE)
plot(novel_outer_polygons, col = "purple", add = TRUE) # this is purple to make the graph easier to read
#combo_plot <- recordPlot() # capture the current plot
#jpeg("E:/Nav Stress Data/dissertation/pics/combo_passive_grids.jpeg", width = 6.5, height = 5.5, units = 'in', res = 500) # save the plot
#replayPlot(combo_plot)
#dev.off()
