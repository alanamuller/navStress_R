library(stringr)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(longitudinalData)
library(RDP)
library(ggpubr)
library(ggplot2)


# Made by Alana Muller with a lot of help from ChatGPT

rm(list = ls())

# City 1 - Store Coordinates
#-----------------------------
# Store 1 - (X,Z): -249.37, 279.16
# Store 2 - (X,Z): 207.3, 99.9
# Store 3 - (X,Z): 145.79, -231.68
# Store 4 - (X,Z): -130.43, -112.92

# City 2 - Store Coordinates
#-----------------------------
# Store 1 - (X,Z): -95.22, 107.39
# Store 2 - (X,Z): 111.44, 246.8
# Store 3 - (X,Z): 169.62, -79.86
# Store 4 - (X,Z): -213.14,	-234.83

# City 3 - Store Coordinates
#-----------------------------
# Store 1 - (X,Z): -152.56, 162.41
# Store 2 - (X,Z): 209.17, 221.62
# Store 3 - (X,Z): 266.59, -234.67
# Store 4 - (X,Z): -249.76, -115.34

# adds index numbers to each cell in overlap analysis in case you want to check something
#  for (i in 1:ncellx) {
#      for (j in 1:ncelly) {
#      cell_index <- (j - 1) * ncellx + i  # Calculate the index of the current cell
#      cell_center_x <- xmin + (i - 0.5) * cellsize
#      cell_center_y <- ymax - (j - 0.5) * cellsize  # Calculate the y-coordinate to move down the rows
#      text(cell_center_x, cell_center_y, labels = cell_index, cex = 0.7)
#    }
#  }

##### Read in file with subject, condition, and city data
setwd("E:/Nav Stress Data/") # set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress shortcuts") # for developing

subj_cond_city_data <- read.xlsx("subj_cond_city.xlsx") # read in file

##### Change this to run next subject

# for my presentation, 1-13 except 4

subject_num <- "1"
subject_city <- "city3" # options are "city1", "city2", and "city3"

# Set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")
setwd(paste("E:/Nav Stress Data/Participant_data/", subject_num, sep = ""))
# setwd("C:/Users/almul/OneDrive/Desktop/Alana/UA/HSCL/Stress Shortcuts")

 
# Load the data
input_file <- paste(subject_num, "_", subject_city, ".log", sep = "")
input_data <- paste(readLines(input_file), collapse="\n")
text <- input_data

# set working directory to save pics - make sure a new folder is created already for the subject's pics
folder_name <- paste("E:/Nav Stress Data/Participant_data/", subject_num, "/pics", sep = "")

setwd(folder_name)

###################### Functions ######################

# function to calculate total distance of the path - aka totDist
totDist <- function(x,y) {
  sum(sqrt(diff(x)^2 + diff(y)^2))
}

### Function to calculate Euclidean distance between two points
euc_dist <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

###################### Define the store points and labels for making graphs ######################
if (subject_city == "city1") {
  store1x <- -249.37
  store1y <- 279.16
  store2x <- 207.3
  store2y <- 99.9
  store3x <- 145.79
  store3y <- -231.68
  store4x <- -130.43
  store4y <- -112.92
} else if (subject_city == "city2"){
  store1x <- -95.22
  store1y <- 107.39
  store2x <- 111.44
  store2y <- 246.8
  store3x <- 169.62
  store3y <- -79.86
  store4x <- -213.14
  store4y <- -234.83
} else if (subject_city == "city3"){
  store1x <- -152.56
  store1y <- 162.41
  store2x <- 209.17
  store2y <- 221.62
  store3x <- 266.59
  store3y <- -234.67
  store4x <- -249.76
  store4y <- -115.34
}

store_coordinates <- list(
  Store1 = c(store1x, store1y), 
  Store2 = c(store2x, store2y), 
  Store3 = c(store3x, store3y), 
  Store4 = c(store4x, store4y)
)



##################################### EXTRACT OUTER PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeOuterPaths and TASK_END TASK_EncodeOuterPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeOuterPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeOuterPaths") # finds the data between the start and end point
outer_df <- data.frame(matches, stringsAsFactors = FALSE) # puts each instance into a dataframe - there should be 2 or 4 observations
colnames(outer_df)[1] <- "learn_outer_paths"

############# Extract all lines between TASK_START LearnSmoothPassive and TASK_END LearnSmoothPassive
outer_passive_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(outer_df)) {
  matches <- str_extract_all(outer_df[i,1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+(.*?)\\s+TASK_END\\s+LearnSmoothPassive")
  outer_passive_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
colnames(outer_passive_df)[1] <- "outer_passive_task" # name the column

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

# Use first trial as the actual whole path length
outer_actual_dist <- totDist(outer_passive_df_list[[1]]$pos_X, outer_passive_df_list[[1]]$pos_Z)


# make and save a graph (only one passive path pic needed)
#p <- ggplot(outer_passive_df_list[[1]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#  geom_point() +
#  scale_color_gradient(low = "lightblue", high = "darkblue") +
#  labs(x = "X", y = "Y", color = "Time (s)", title = "Outer Path Passive Learning 1") +
#  theme(plot.title = element_text(hjust = 0.5, size = 16), 
#        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#  geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
#  geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
#  geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
#  geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
#  geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
#  geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
#  geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
#  geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")

#ggsave(paste0(subject_city, "_", "outer_passive", ".jpg"), p, width = 6.5, height = 5.5, units = 'in', dpi = 500)

############# Extract all lines between TASK_START LearnActivePath and TASK_END LearnActivePath
outer_active_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(outer_df)) {
  matches <- str_extract_all(outer_df[i,1], "(?s)TASK_START\\s+LearnActivePath\\s+(.*?)\\s+TASK_END\\s+LearnActivePath")
  outer_active_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
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

# Make a save plots for each active learning trial (to make sure they followed directions)
#for (i in seq_along(outer_active_df_list)) {
# Create the plot title name
#  plot_title <- paste("Outer Path Active Learning" , i, subject_city, sep = " ")
# Create plot
#  gg <- ggplot(outer_active_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#   geom_point() +
#   scale_color_gradient(low = "lightblue", high = "darkblue") +
#   labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
#   theme(plot.title = element_text(hjust = 0.5, size = 16), 
#         axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#         legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#    geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
#    geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
#    geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
#    geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
#    geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
#    geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
#    geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
#    geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")

# Save the plot
# ggsave(paste0(subject_city, "_", "outer_active_", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
#}

##################################### EXTRACT Inner PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeInnerPaths and TASK_END TASK_EncodeInnerPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeInnerPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeInnerPaths") # finds the data between the start and end point
inner_df <- data.frame(matches, stringsAsFactors = FALSE) # puts each instance into a dataframe - there should be 2 or 4 observations
colnames(inner_df)[1] <- "learn_Inner_paths"

############# Extract all lines between TASK_START LearnSmoothPassive and TASK_END LearnSmoothPassive
inner_passive_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(inner_df)) {
  matches <- str_extract_all(inner_df[i,1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+(.*?)\\s+TASK_END\\s+LearnSmoothPassive")
  inner_passive_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
colnames(inner_passive_df)[1] <- "inner_passive_task" # name the column

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

# Use first trial as the actual whole path length
inner_actual_dist <- totDist(inner_passive_df_list[[1]]$pos_X, inner_passive_df_list[[1]]$pos_Z)


# make and save a graph (only one passive path needed)
#p <- ggplot(inner_passive_df_list[[1]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#  geom_point() +
#  scale_color_gradient(low = "lightblue", high = "darkblue") +
#  labs(x = "X", y = "Y", color = "Time (s)", title = "Inner Path Passive Learning 1") +
#  theme(plot.title = element_text(hjust = 0.5, size = 16), 
#        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#  geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
#  geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
#  geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
#  geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
#  geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
#  geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
#  geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
#  geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")

#ggsave(paste0(subject_city, "_", "inner_passive", ".jpg"), p, width = 6.5, height = 5.5, units = 'in', dpi = 500)


############# Extract all lines between TASK_START LearnActivePath and TASK_END LearnActivePath
inner_active_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(inner_df)) {
  matches <- str_extract_all(inner_df[i,1], "(?s)TASK_START\\s+LearnActivePath\\s+(.*?)\\s+TASK_END\\s+LearnActivePath")
  inner_active_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
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

# Make a save plots for each active learning trial (to make sure they followed directions)
#for (i in seq_along(inner_active_df_list)) {
  # Create the plot title name
#  plot_title <- paste("Inner Path Active Learning" , i, subject_city, sep = " ")
  # Create plot
#  gg <- ggplot(inner_active_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#    geom_point() +
#    scale_color_gradient(low = "lightblue", high = "darkblue") +
#    labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
#    theme(plot.title = element_text(hjust = 0.5, size = 16), 
#          axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#          legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#    geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
#    geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
#    geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
#    geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
#    geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
#    geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
#    geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
#    geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")
  
  # Save the plot
#  ggsave(paste0(subject_city, "_", "inner_active_", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
#}

##################################### EXTRACT PATH RECREATION DATA #####################################

############# Extract all lines between TASK_START TASK_ExplorationTask and TASK_END TASK_ExplorationTask

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_ExplorationTask\\s+(.*?)\\s+TASK_END\\s+TASK_ExplorationTask") # finds the data between the start and end point
recreatePath_df <- data.frame(matches, stringsAsFactors = FALSE) # puts each instance into a dataframe - there should be 2 or 4 observations
colnames(recreatePath_df)[1] <- "recreate_paths"

recreatePath_df_list <- lapply(seq_len(nrow(recreatePath_df)), function(i) data.frame(value = recreatePath_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(recreatePath_df_list)) {
  
  # Get the dataframe from the list
  data_df <- recreatePath_df_list[[i]]
  
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
  data_df$trialname <- paste0("recreatePath", i)
  
  # Update the dataframe in the list
  recreatePath_df_list[[i]] <- data_df
  
}

# Loop through each path recreation, make and save a plot for each one

#for (i in seq_along(recreatePath_df_list)) {
  # Create the plot title name
#  plot_title <- paste("Path Recreation", i, subject_city, sep = " ")
  # Create plot
#  gg <- ggplot(recreatePath_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#   geom_point() +
#   scale_color_gradient(low = "lightblue", high = "darkblue") +
#   labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
#   theme(plot.title = element_text(hjust = 0.5, size = 16), 
#         axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#         legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#    geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
#    geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
#    geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
#    geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
#    geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
#    geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
#    geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
#    geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")
  
  # Save the plot
# ggsave(paste0(subject_city, "_", "path_recreate", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
#}

##################################### RETRIEVE: NAVIGATION TASK #####################################

############# Extract all lines between TASK_START NavigationTrials and TASK_END NavigationTrials

matches <- str_extract_all(text, "(?s)TASK_START\\s+NavigationTrials\\s+(.*?)\\s+TASK_END\\s+NavigationTrials") # finds the data between the start and end point
navTestBlocks_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunk of data 
colnames(navTestBlocks_df)[1] <- "navTestBlocks" # renames the first column

############# Extract all lines between TASK_START	Navigate and TASK_END	Navigate

matches <- str_extract_all(text, "(?s)TASK_START\\s+Navigate\\s+(.*?)\\s+TASK_END\\s+Navigate") # finds the data between the start and end point
navTestTrials_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunk of data 
colnames(navTestTrials_df)[1] <- "navTestTrials" # renames the first column

navTestTrials_df_list <- lapply(seq_len(nrow(navTestTrials_df)), function(i) data.frame(value = navTestTrials_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(navTestTrials_df_list)) {
  
  # Get the dataframe from the list
  data_df <- navTestTrials_df_list[[i]]
  
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
  navTestTrials_df_list[[i]] <- data_df
  
}

# make one big dataframe with all nav trials' x z values to make a graph
#navTest_all_dfs <- do.call(rbind, navTestTrials_df_list)

#all_nav_plot <- ggplot(navTest_all_dfs, aes(x = pos_X, y = pos_Z, color = time_sec)) +
# geom_point() +
# scale_color_gradient(low = "lightblue", high = "darkblue") +
# labs(x = "X", y = "Y", color = "Time (s)", title = paste("All Navigation Test Trials", subject_city, sep = " ")) +
# theme(plot.title = element_text(hjust = 0.5, size = 20), 
#       axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#       legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
# coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
# scale_y_continuous(breaks = seq(-400,400,100)) +
# scale_x_continuous(breaks = seq(-400,400,100)) + 
# geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
# geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
# geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
# geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
# geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
# geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
# geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
# geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")
  
#ggsave(paste0(subject_city, "_", "all_navTest_trials_", subject_num, ".jpeg"), all_nav_plot, width = 6.5, height = 5.5, units = 'in', dpi = 500)


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

############### Add store numbers to log_data

# Make a duplicate column of Navigate_target so we can replace the names with store numbers
log_data$target_store_num <- log_data$Navigate_target

# Get store names for Targets 01-04
pattern <- "Target01:\\s*(.*?)\\s*Position:" # Find the words between "Target01:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target01:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target01:\\s*|\\s*Position:", "", combined_matches) # Remove "Target01:" and "Position:" from the combined matches
Target01 <- extracted_text
replaceTarget01 <- "Store1"

pattern <- "Target02:\\s*(.*?)\\s*Position:" # Find the words between "Target02:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target02:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target02:\\s*|\\s*Position:", "", combined_matches) # Remove "Target02:" and "Position:" from the combined matches
Target02 <- extracted_text
replaceTarget02 <- "Store2"

pattern <- "Target03:\\s*(.*?)\\s*Position:" # Find the words between "Target03:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target03:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target03:\\s*|\\s*Position:", "", combined_matches) # Remove "Target03:" and "Position:" from the combined matches
Target03 <- extracted_text
replaceTarget03 <- "Store3"

pattern <- "Target04:\\s*(.*?)\\s*Position:" # Find the words between "Target04:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target04:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target04:\\s*|\\s*Position:", "", combined_matches) # Remove "Target04:" and "Position:" from the combined matches
Target04 <- extracted_text
replaceTarget04 <- "Store4"

# Replace Target number with Store number
log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target01, replaceTarget01, target_store_num))

log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target02, replaceTarget02, target_store_num))

log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target03, replaceTarget03, target_store_num))

log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target04, replaceTarget04, target_store_num))

# Add column to indicate the starting and ending stores to categorize trials
log_data$startEnd_store <- paste(lag(log_data$target_store_num), log_data$target_store_num, sep = " ")

# Fix the first entry in the startEnd_store column
# Cities 1 and 2 are fine but city 3 got messed up a little until Subject 23 when it was fixed
# So find the store closest to the beginning point and label it that

# define the starting point
starting_point <- c(navTestTrials_df_list[[1]]$pos_X[1], navTestTrials_df_list[[1]]$pos_Z[1])

# calculate distances from starting opint to each store
distances <- sapply(store_coordinates, function(store_coord){
  euc_dist(starting_point, store_coord)
})

# find the name of the closest store
closest_store <- names(distances)[which.min(distances)]

# combine the names and add to data sheet
log_data$startEnd_store[1] <- paste(closest_store, log_data$target_store_num[1], sep = " ")

##### Add a column to the log_data sheet indicating the condition
# Get the condition name from the subj_cond_city_data
condition_name <- subj_cond_city_data$condition[subj_cond_city_data$subjectID == as.numeric(subject_num) & subj_cond_city_data$cityNum == subject_city]

# Get the more familiar path form the subj_cond_city_data
more_familiar_path <- subj_cond_city_data$moreFamiliarPath[subj_cond_city_data$subjectID == as.numeric(subject_num) & subj_cond_city_data$cityNum == subject_city]

log_data$condition <- condition_name # add the column to log_data
log_data$moreFamiliarPath <- more_familiar_path

log_data$city <- subject_city

####################### Make another dataframe with recreated path trials: total path length, excess path outer and inner, and duration values

# Make an empty dataframe to put the data into
recreate_paths_log <- data.frame(trialname = character(), total_path_distance = numeric(), excess_path_outer_dist = numeric(), 
                                 excess_path_inner_dist = numeric(), path_duration = numeric())

for (i in 1:length(recreatePath_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(recreatePath_df_list[[i]]$pos_X, recreatePath_df_list[[i]]$pos_Z)
  
  # calculate excess path distance with outer path
  excess_path_outer = path_dist - outer_actual_dist
  
  # calculate excess path distance with inner path
  excess_path_inner = path_dist - inner_actual_dist
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(recreatePath_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  recreate_paths_log <- rbind(recreate_paths_log, data.frame(trialname = recreatePath_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, 
                                                             excess_path_outer_dist = excess_path_outer, excess_path_inner_dist = excess_path_inner, 
                                                             path_duration = path_dur))
}

recreate_paths_log$subjectID <- subject_num
recreate_paths_log$condition <- condition_name
recreate_paths_log$moreFamiliarPath <- more_familiar_path
recreate_paths_log$city <- subject_city

####################### Make another dataframe with learning trials: total path length, excess path length, and duration values
# But this total path distance is comparing the learned path to the traveled path

# Make an empty dataframe to put the path distance values into
learning_path_log <- data.frame(trialname = character(), total_path_distance = numeric(), excess_path_distance = numeric(), path_duration = numeric(), stringsAsFactors = FALSE)

########## OUTER PASSIVE ##########
for (i in 1:length(outer_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_passive_df_list[[i]]$pos_X, outer_passive_df_list[[i]]$pos_Z)
  
  # no excess path distance to calculate here
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  learning_path_log <- rbind(learning_path_log, data.frame(trialname = outer_passive_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = "N/A", path_duration = path_dur))
  
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
  learning_path_log <- rbind(learning_path_log, data.frame(trialname = outer_active_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

########## INNER PASSIVE ##########
for (i in 1:length(inner_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_passive_df_list[[i]]$pos_X, inner_passive_df_list[[i]]$pos_Z)
  
  # no excess path distance to calculate here

  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  learning_path_log <- rbind(learning_path_log, data.frame(trialname = inner_passive_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = "N/A", path_duration = path_dur))
  
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
  learning_path_log <- rbind(learning_path_log, data.frame(trialname = inner_active_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

learning_path_log$subjectID <- subject_num
learning_path_log$condition <- condition_name
learning_path_log$moreFamiliarPath <- more_familiar_path
learning_path_log$city <- subject_city

####################### Make plots for each nav test trial #######################

# loop through a dataframe list to generate a plot for each trial
for (i in seq_along(navTestTrials_df_list)) {
  # create the plot title name
  plot_title <- paste("Navigation Test Trial", i, subject_city, sep = " ")
  # create the ggplot object for the current data frame
  gg <- ggplot(navTestTrials_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
    geom_point() +
    scale_color_gradient(low = "lightblue", high = "darkblue") +
    labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
          legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
    coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
    scale_y_continuous(breaks = seq(-400,400,100)) +
    scale_x_continuous(breaks = seq(-400,400,100)) +
    geom_point(aes(x = store1x, y = store1y), size = 4, color = "red") +
    geom_point(aes(x = store2x, y = store2y), size = 4, color = "red") +
    geom_point(aes(x = store3x, y = store3y), size = 4, color = "red") +
    geom_point(aes(x = store4x, y = store4y), size = 4, color = "red") +
    geom_text(aes(x = store1x - 30, y = store1y + 20, label = "Store 1"), size = 4, color = "black") +
    geom_text(aes(x = store2x + 30, y = store2y + 20, label = "Store 2"), size = 4, color = "black") +
    geom_text(aes(x = store3x + 30, y = store3y - 20, label = "Store 3"), size = 4, color = "black") +
    geom_text(aes(x = store4x - 30, y = store4y - 20, label = "Store 4"), size = 4, color = "black")
  
  # save the plot with a file name based on the index of the data frame
  ggsave(paste0(subject_city, "_", "navTest_trial", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
}

################################################################################
######################## Calculate overlapping grids ###########################

library(sp)
library(raster)
library(sf)

# define the extent of the area you want to cover - used the corners of the environment output by unity
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
grid_poly <- as(grid_sp, "SpatialPolygons") # need for making plots

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

# Define the group of indices you want to color for making plots
indices_outer_red <- unique_outerPath_grids
indices_inner_red <- unique_innerPath_grids
indices_innerOuterOverlap_gray <- inner_outer_overlap

#  Extract polygons corresponding to the selected indices
outer_red <- grid_poly[indices_outer_red]
inner_red <- grid_poly[indices_inner_red]
innerOuter_overlap_gray <- grid_poly[indices_innerOuterOverlap_gray]

##### Add to the recreate_paths_log file with overlapping block numbers

for (i in 1:length(recreatePath_df_list)) {
  recreate_df <- data.frame(x = recreatePath_df_list[[i]]$pos_X, y = recreatePath_df_list[[i]]$pos_Z)
  recreate_sp <- SpatialPoints(recreate_df)
  
  # find the correct grid index for each x,z path value 
  recreate_grid <- over(recreate_sp, grid_sp)
  
  # find the unique indices for the path (unique block indices)
  unique_recreate_grids <- unique(recreate_grid) # use this for making plots
  
  # remove the segments of the inner and outer path that overlap # this is for the data analysis
  no_overlap_unique_recreate_grids <- unique_recreate_grids[!unique_recreate_grids %in% inner_outer_overlap]
  
  # find the total amount of unique grids each path uses
  recreate_grid_total <- length(no_overlap_unique_recreate_grids)
  
  # find overlap with outer path and length - will add to data log
  overlap_outer_indices <- intersect(unique_outerPath_grids_no_Overlap, no_overlap_unique_recreate_grids)
  overlap_outer <- length(overlap_outer_indices)
  
  # find overlap with inner path and length - will add to data log
  overlap_inner_indices <- intersect(unique_innerPath_grids_no_Overlap, no_overlap_unique_recreate_grids)
  overlap_inner <- length(overlap_inner_indices)
  
  # add the three numbers above to the data log
  recreate_paths_log$grid_count[i] <- recreate_grid_total
  recreate_paths_log$grid_overlap_outer[i] <- overlap_outer
  recreate_paths_log$grid_overlap_inner[i] <- overlap_inner
  
  # add column to categorize recreated path as inner or outer
  if (recreate_paths_log$grid_overlap_outer[i] > recreate_paths_log$grid_overlap_inner[i]){
    recreate_paths_log$path_recreated[i] <- "outer"
  } else {
    recreate_paths_log$path_recreated[i] <- "inner"
  }
  
  # calculate the number of novel blocks they traveled
  recreate_paths_log$novel_grids[i] <- recreate_grid_total - overlap_outer - overlap_inner
  
  # Define the group of indices you want to color for making plots
  indices_recreate_blue <- unique_recreate_grids
  indices_overlapOuter_purple <- overlap_outer_indices
  indices_overlapInner_purple <- overlap_inner_indices

  # Extract polygons corresponding to the selected indices
  recreate_blue <- grid_poly[indices_recreate_blue]
  overlapOuter_purple <- grid_poly[indices_overlapOuter_purple]
  overlapInner_purple <- grid_poly[indices_overlapInner_purple]
  
  ### Make a plot for outer path overlap
#  if (recreate_paths_log$path_recreated[i] == "outer") {
#    grid_plot_title <- paste("Recreated", recreate_paths_log$path_recreated[i], "path",  i, subject_city, sep = " ")
#    plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE, main = grid_plot_title)
#    lines(grid_poly, col = "gray", add = TRUE)
    
    # Plot the selected polygons
#    plot(outer_red, col = "red", add = TRUE)
#    plot(recreate_blue, col = "blue", add = TRUE)
#    plot(overlapOuter_purple, col = "purple", add = TRUE)
#    plot(innerOuter_overlap_gray, col = "purple", add = TRUE) # this is purple to make the graph easier to read
    
#    outerOverlap_plot <- recordPlot() # capture the current plot
    
#    jpeg(paste(subject_city, "_", "Overlap_outer_recreated_", i, ".jpeg", sep = ""), width = 6.5, height = 5.5, units = 'in', res = 500) # save the plot
#    replayPlot(outerOverlap_plot)
#    dev.off()
#  } else if (recreate_paths_log$path_recreated[i] == "inner") {
      ### Make a plot for inner path overlap
#  grid_plot_title <- paste("Recreated", recreate_paths_log$path_recreated[i], "path",  i, subject_city, sep = " ")
#  plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE, main = grid_plot_title)
#  lines(grid_poly, col = "gray", add = TRUE)
  
  # Plot the selected polygons
#  plot(inner_red, col = "red", add = TRUE)
#  plot(recreate_blue, col = "blue", add = TRUE)
#  plot(overlapInner_purple, col = "purple", add = TRUE)
#  plot(innerOuter_overlap_gray, col = "purple", add = TRUE) # this is purple to make the graph easier to read
  
#  innerOverlap_plot <- recordPlot() # capture the current plot
  
#  jpeg(paste(subject_city, "_", "Overlap_inner_recreated_", i, ".jpeg", sep = ""), width = 6.5, height = 5.5, units = 'in', res = 500) # save the plot
#  replayPlot(innerOverlap_plot)
#  dev.off()
#  }
}


################### For navigation test trials ###################


##################### Getting the closest points to separate the whole passive path into four segments #####################
##################### To find out which paths people found the shortcuts

# Define the points to search for depending on city number
if (subject_city == "city1"){
  search_points <- data.frame(x = c(-249.37, 207.3, 145.79, -130.43), y = c(279.16, 99.9, -231.68, -112.92))
} else if (subject_city == "city2"){
  search_points <- data.frame(x = c(-95.22, 111.44, 169.62, -213.14), y = c(107.39, 246.8, -79.86, -234.83))
} else if (subject_city == "city3"){
  search_points <- data.frame(x = c(-152.56, 209.17, 266.59, -249.76), y = c(162.41, 221.62, -234.67, -115.34))
}

########### OUTER PATH ###########

# Find the closest point to each search point
outer_closest_points <- lapply(1:nrow(search_points), function(i) {
  
  # Calculate distances to all points in data frame
  distances <- sqrt((outer_passive_df_list[[length(outer_passive_df_list)]]$pos_X - search_points[i, "x"])^2 + (outer_passive_df_list[[length(outer_passive_df_list)]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = outer_passive_df_list[[length(outer_passive_df_list)]]$pos_X[min_index], 
                    y = outer_passive_df_list[[length(outer_passive_df_list)]]$pos_Z[min_index], 
                    index = min_index,
                    time = outer_passive_df_list[[length(outer_passive_df_list)]]$time_sec[min_index]))
})

# Combine the closest points into a single data frame
outer_closest_points_df <- do.call(rbind, outer_closest_points)

### Make a list of dataframes to segment one path into four paths

# Initialize the dataframe
outer_passive_seg_list <- list()

# Store 1 to store 2
outer_passive_seg_list[[1]] <- outer_passive_df_list[[length(outer_passive_df_list)]] %>%
  filter(time_sec <= outer_closest_points_df$time[2])
# Store 2 to store 3
outer_passive_seg_list[[2]] <- outer_passive_df_list[[length(outer_passive_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[2] & time_sec <= outer_closest_points_df$time[3])
# Store 3 to store 4
outer_passive_seg_list[[3]] <- outer_passive_df_list[[length(outer_passive_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[3] & time_sec <= outer_closest_points_df$time[4])
# Store 4 to store 1
outer_passive_seg_list[[4]] <- outer_passive_df_list[[length(outer_passive_df_list)]] %>%
  filter(time_sec > outer_closest_points_df$time[4])

# Make an empty dataframe to put the path segment distance values into
outer_actual_seg_dist <- data.frame(segment_distance = numeric(), stringsAsFactors = FALSE)

# Fill dataframe with actual path distance values for each segment # Might not need this anymore
for (i in seq_along(outer_passive_seg_list)) {
  actual_seg_dist <- totDist(outer_passive_seg_list[[i]]$pos_X, outer_passive_seg_list[[i]]$pos_Z)
  
  # put info in dataframe
  outer_actual_seg_dist <- rbind(outer_actual_seg_dist, data.frame(segment_distance = actual_seg_dist))
}

##### Store 1 to Store 2 path
out_s1s2_df <- data.frame(x = outer_passive_seg_list[[1]]$pos_X, y = outer_passive_seg_list[[1]]$pos_Z) # define the dataframe
out_s1s2_sp <- SpatialPoints(out_s1s2_df) # turn it into spatial data
out_s1s2_grid <- over(out_s1s2_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_out_s1s2_grids <- unique(out_s1s2_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_out_s1s2 <- length(unique_out_s1s2_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_out_s1s2_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Store 2 to Store 3 path
out_s2s3_df <- data.frame(x = outer_passive_seg_list[[2]]$pos_X, y = outer_passive_seg_list[[2]]$pos_Z) # define the dataframe
out_s2s3_sp <- SpatialPoints(out_s2s3_df) # turn it into spatial data
out_s2s3_grid <- over(out_s2s3_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_out_s2s3_grids <- unique(out_s2s3_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_out_s2s3 <- length(unique_out_s2s3_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_out_s2s3_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Store 3 to Store 4 path
out_s3s4_df <- data.frame(x = outer_passive_seg_list[[3]]$pos_X, y = outer_passive_seg_list[[3]]$pos_Z) # define the dataframe
out_s3s4_sp <- SpatialPoints(out_s3s4_df) # turn it into spatial data
out_s3s4_grid <- over(out_s3s4_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_out_s3s4_grids <- unique(out_s3s4_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_out_s3s4 <- length(unique_out_s3s4_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_out_s3s4_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Store 4 to Store 1 path
out_s4s1_df <- data.frame(x = outer_passive_seg_list[[4]]$pos_X, y = outer_passive_seg_list[[4]]$pos_Z) # define the dataframe
out_s4s1_sp <- SpatialPoints(out_s4s1_df) # turn it into spatial data
out_s4s1_grid <- over(out_s4s1_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_out_s4s1_grids <- unique(out_s4s1_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_out_s4s1 <- length(unique_out_s4s1_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_out_s4s1_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

########### INNER PATH ###########

# Find the closest point to each search point
inner_closest_points <- lapply(1:nrow(search_points), function(i) {
  
  # Calculate distances to all points in data frame
  distances <- sqrt((inner_passive_df_list[[length(inner_passive_df_list)]]$pos_X - search_points[i, "x"])^2 + (inner_passive_df_list[[length(inner_passive_df_list)]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = inner_passive_df_list[[length(inner_passive_df_list)]]$pos_X[min_index], 
                    y = inner_passive_df_list[[length(inner_passive_df_list)]]$pos_Z[min_index], 
                    index = min_index,
                    time = inner_passive_df_list[[length(inner_passive_df_list)]]$time_sec[min_index]))
})

# Combine the closest points into a single data frame
inner_closest_points_df <- do.call(rbind, inner_closest_points)

### Make a list of dataframes to segment one path into four paths

# Initialize the dataframe
inner_passive_seg_list <- list()

# Store 1 to store 2
inner_passive_seg_list[[1]] <- inner_passive_df_list[[length(inner_passive_df_list)]] %>%
  filter(time_sec <= inner_closest_points_df$time[2])
# Store 2 to store 3
inner_passive_seg_list[[2]] <- inner_passive_df_list[[length(inner_passive_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[2] & time_sec <= inner_closest_points_df$time[3])
# Store 3 to store 4
inner_passive_seg_list[[3]] <- inner_passive_df_list[[length(inner_passive_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[3] & time_sec <= inner_closest_points_df$time[4])
# Store 4 to store 1
inner_passive_seg_list[[4]] <- inner_passive_df_list[[length(inner_passive_df_list)]] %>%
  filter(time_sec > inner_closest_points_df$time[4])

# Make an empty dataframe to put the path segment distance values into
inner_actual_seg_dist <- data.frame(segment_distance = numeric(), stringsAsFactors = FALSE)

# Fill dataframe with actual path distance values for each segment # Might not need this anymore
for (i in seq_along(inner_passive_seg_list)) {
  actual_seg_dist <- totDist(inner_passive_seg_list[[i]]$pos_X, inner_passive_seg_list[[i]]$pos_Z)
  
  # put info in dataframe
  inner_actual_seg_dist <- rbind(inner_actual_seg_dist, data.frame(segment_distance = actual_seg_dist))
}

##### Store 1 to Store 2 path
in_s1s2_df <- data.frame(x = inner_passive_seg_list[[1]]$pos_X, y = inner_passive_seg_list[[1]]$pos_Z) # define the dataframe
in_s1s2_sp <- SpatialPoints(in_s1s2_df) # turn it into spatial data
in_s1s2_grid <- over(in_s1s2_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_in_s1s2_grids <- unique(in_s1s2_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_in_s1s2 <- length(unique_in_s1s2_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_in_s1s2_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Store 2 to Store 3 path
in_s2s3_df <- data.frame(x = inner_passive_seg_list[[2]]$pos_X, y = inner_passive_seg_list[[2]]$pos_Z) # define the dataframe
in_s2s3_sp <- SpatialPoints(in_s2s3_df) # turn it into spatial data
in_s2s3_grid <- over(in_s2s3_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_in_s2s3_grids <- unique(in_s2s3_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_in_s2s3 <- length(unique_in_s2s3_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_in_s2s3_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Store 3 to Store 4 path
in_s3s4_df <- data.frame(x = inner_passive_seg_list[[3]]$pos_X, y = inner_passive_seg_list[[3]]$pos_Z) # define the dataframe
in_s3s4_sp <- SpatialPoints(in_s3s4_df) # turn it into spatial data
in_s3s4_grid <- over(in_s3s4_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_in_s3s4_grids <- unique(in_s3s4_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_in_s3s4 <- length(unique_in_s3s4_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_in_s3s4_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Store 4 to Store 1 path
in_s4s1_df <- data.frame(x = inner_passive_seg_list[[4]]$pos_X, y = inner_passive_seg_list[[4]]$pos_Z) # define the dataframe
in_s4s1_sp <- SpatialPoints(in_s4s1_df) # turn it into spatial data
in_s4s1_grid <- over(in_s4s1_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
unique_in_s4s1_grids <- unique(in_s4s1_grid) # find the unique numbers for each of the paths representing the total grids the path uses
grid_total_in_s4s1 <- length(unique_in_s4s1_grids) # find the total number of grids each path uses
indices_to_color_red <- unique_in_s4s1_grids # Define the group of indices you want to color red
selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

##### Add overlapping blocks columns to log data

for (i in 1:length(navTestTrials_df_list)) {
  path_df <- data.frame(x = navTestTrials_df_list[[i]]$pos_X, y = navTestTrials_df_list[[i]]$pos_Z)
  path_sp <- SpatialPoints(path_df)
  
  # find the correct grid index for each x,z path value 
  path_grid <- over(path_sp, grid_sp)
  
  # find the unique indices for the path (unique block indices)
  unique_path_grids <- unique(path_grid) # use this for making plots
  path_grid_tot_w9InOutOverlaps <- length(unique_path_grids)
  
  # remove the segments of the inner and outer path that overlap # this is for the data analysis
  no_overlap_unique_path_grids <- unique_path_grids[!unique_path_grids %in% inner_outer_overlap]
  
  # find the total amount of unique grids each path uses
  path_grid_total <- length(no_overlap_unique_path_grids)
  
  # find overlap with each outer store segment path and length - will add to data log
  overlap_outer_seg1 <- intersect(no_overlap_unique_path_grids, unique_out_s1s2_grids)
  overlap_outer_seg2 <- intersect(no_overlap_unique_path_grids, unique_out_s2s3_grids)
  overlap_outer_seg3 <- intersect(no_overlap_unique_path_grids, unique_out_s3s4_grids)
  overlap_outer_seg4 <- intersect(no_overlap_unique_path_grids, unique_out_s4s1_grids)
  
  overlap_outer_seg1_num <- length(overlap_outer_seg1)
  overlap_outer_seg2_num <- length(overlap_outer_seg2)
  overlap_outer_seg3_num <- length(overlap_outer_seg3)
  overlap_outer_seg4_num <- length(overlap_outer_seg4)
  
  # find overlap with each inner store segment path and length - will add to data log
  overlap_inner_seg1 <- intersect(no_overlap_unique_path_grids, unique_in_s1s2_grids)
  overlap_inner_seg2 <- intersect(no_overlap_unique_path_grids, unique_in_s2s3_grids)
  overlap_inner_seg3 <- intersect(no_overlap_unique_path_grids, unique_in_s3s4_grids)
  overlap_inner_seg4 <- intersect(no_overlap_unique_path_grids, unique_in_s4s1_grids)
  
  overlap_inner_seg1_num <- length(overlap_inner_seg1)
  overlap_inner_seg2_num <- length(overlap_inner_seg2)
  overlap_inner_seg3_num <- length(overlap_inner_seg3)
  overlap_inner_seg4_num <- length(overlap_inner_seg4)
  
  # add the numbers above to the data log
  log_data$grid_count_w9InOutOverlaps[i] <- path_grid_tot_w9InOutOverlaps
  log_data$grid_count[i] <- path_grid_total
  
  log_data$grid_overlap_outer_seg1[i] <- overlap_outer_seg1_num
  log_data$grid_overlap_outer_seg2[i] <- overlap_outer_seg2_num  
  log_data$grid_overlap_outer_seg3[i] <- overlap_outer_seg3_num
  log_data$grid_overlap_outer_seg4[i] <- overlap_outer_seg4_num
  
  log_data$grid_overlap_inner_seg1[i] <- overlap_inner_seg1_num
  log_data$grid_overlap_inner_seg2[i] <- overlap_inner_seg2_num
  log_data$grid_overlap_inner_seg3[i] <- overlap_inner_seg3_num
  log_data$grid_overlap_inner_seg4[i] <- overlap_inner_seg4_num
  
  # calculate the number of novel blocks they traveled
  log_data$novel_grids[i] <- path_grid_total - sum(overlap_outer_seg1_num, overlap_outer_seg2_num, overlap_outer_seg3_num,
                                                   overlap_outer_seg4_num, overlap_inner_seg1_num, overlap_inner_seg2_num, 
                                                   overlap_inner_seg3_num, overlap_inner_seg4_num)
}

log_data <- as.data.frame(log_data)

# add optimum segment length counted in excel sheet
for (i in 1:nrow(log_data)) {
  if (subject_city == "city1") {
    if (log_data$startEnd_store[i] == "Store1 Store2" | log_data$startEnd_store[i] == "Store2 Store1"){
      log_data$optimal_block_num[i] <- 33
    } else if (log_data$startEnd_store[i] == "Store2 Store3" | log_data$startEnd_store[i] == "Store3 Store2"){
      log_data$optimal_block_num[i] <- 20
    } else if (log_data$startEnd_store[i] == "Store3 Store4" | log_data$startEnd_store[i] == "Store4 Store3"){
      log_data$optimal_block_num[i] <- 20
    } else if (log_data$startEnd_store[i] == "Store4 Store1" | log_data$startEnd_store[i] == "Store1 Store4"){
      log_data$optimal_block_num[i] <- 26
    } else if (log_data$startEnd_store[i] == "Store1 Store3" | log_data$startEnd_store[i] == "Store3 Store1"){
      log_data$optimal_block_num[i] <- 45
    } else if (log_data$startEnd_store[i] == "Store2 Store4" | log_data$startEnd_store[i] == "Store4 Store2"){
      log_data$optimal_block_num[i] <- 28
    }
    
  } else if (subject_city == "city2") {
    if (log_data$startEnd_store[i] == "Store1 Store2" | log_data$startEnd_store[i] == "Store2 Store1"){
      log_data$optimal_block_num[i] <- 21
    } else if (log_data$startEnd_store[i] == "Store2 Store3" | log_data$startEnd_store[i] == "Store3 Store2"){
      log_data$optimal_block_num[i] <- 21
    } else if (log_data$startEnd_store[i] == "Store3 Store4" | log_data$startEnd_store[i] == "Store4 Store3"){
      log_data$optimal_block_num[i] <- 27
    } else if (log_data$startEnd_store[i] == "Store4 Store1" | log_data$startEnd_store[i] == "Store1 Store4"){
      log_data$optimal_block_num[i] <- 23
    } else if (log_data$startEnd_store[i] == "Store1 Store3" | log_data$startEnd_store[i] == "Store3 Store1"){
      log_data$optimal_block_num[i] <- 26
    } else if (log_data$startEnd_store[i] == "Store2 Store4" | log_data$startEnd_store[i] == "Store4 Store2"){
      log_data$optimal_block_num[i] <- 42
    }
    
  } else if (subject_city == "city3") {
    if (log_data$startEnd_store[i] == "Store1 Store2" | log_data$startEnd_store[i] == "Store2 Store1"){
      log_data$optimal_block_num[i] <- 22
    } else if (log_data$startEnd_store[i] == "Store2 Store3" | log_data$startEnd_store[i] == "Store3 Store2"){
      log_data$optimal_block_num[i] <- 26
    } else if (log_data$startEnd_store[i] == "Store3 Store4" | log_data$startEnd_store[i] == "Store4 Store3"){
      log_data$optimal_block_num[i] <- 32
    } else if (log_data$startEnd_store[i] == "Store4 Store1" | log_data$startEnd_store[i] == "Store1 Store4"){
      log_data$optimal_block_num[i] <- 19
    } else if (log_data$startEnd_store[i] == "Store1 Store3" | log_data$startEnd_store[i] == "Store3 Store1"){
      log_data$optimal_block_num[i] <- 41
    } else if (log_data$startEnd_store[i] == "Store2 Store4" | log_data$startEnd_store[i] == "Store4 Store2"){
      log_data$optimal_block_num[i] <- 40
    }
  }
}

# create an excess block number column
log_data$excess_block_num <- log_data$grid_count_w9InOutOverlaps - log_data$optimal_block_num

# add calculation for type of path (forward, backward, diagonal)
for (i in 1:nrow(log_data)){
  if (log_data$startEnd_store[i] == "Store1 Store2" | log_data$startEnd_store[i] == "Store2 Store3" | log_data$startEnd_store[i] == "Store3 Store4" | log_data$startEnd_store[i] == "Store4 Store1"){
    log_data$trial_type[i] <- "forward"
  } else if (log_data$startEnd_store[i] == "Store2 Store1" | log_data$startEnd_store[i] == "Store3 Store2" | log_data$startEnd_store[i] == "Store4 Store3" | log_data$startEnd_store[i] == "Store1 Store4"){
    log_data$trial_type[i] <- "backward"
  } else if (log_data$startEnd_store[i] == "Store1 Store3" | log_data$startEnd_store[i] == "Store3 Store1" | log_data$startEnd_store[i] == "Store2 Store4" | log_data$startEnd_store[i] == "Store4 Store2"){
    log_data$trial_type[i] <- "diagonal"
  }}


#test_df <- data.frame(x = navTestTrials_df_list[[25]]$pos_X, y = navTestTrials_df_list[[25]]$pos_Z) # define the dataframe
#test_sp <- SpatialPoints(test_df) # turn it into spatial data
#test_grid <- over(test_sp, grid_sp) # use the "over()" function to find which grids contain the x-y coordinates
#unique_test_grids <- unique(test_grid) # find the unique numbers for each of the paths representing the total grids the path uses
#grid_total <- length(unique_test_grids) # find the total number of grids each path uses
#indices_to_color_red <- unique_test_grids # Define the group of indices you want to color red
#selected_polygons_red <- grid_poly[indices_to_color_red] # Extract polygons corresponding to the selected indices

# Plot the grid
#plot(area_poly, xlim = c(xmin, xmax), ylim = c(ymin, ymax), axes = TRUE) # plot axes
#lines(grid_poly, col = "gray", add = TRUE) # plot grid
#plot(selected_polygons_red, col = "red", add = TRUE) # plot the selected polygons in red

################################# Save log files #################################
# Save log_data to csv file
write.csv(log_data, paste("E:/Nav Stress Data/Participant_data/navTrialsLogData", 
                          subject_num, "_", subject_city, ".csv", sep = ""), row.names = FALSE)

# Save learning_path_log to csv file
write.csv(learning_path_log, paste("E:/Nav Stress Data/Participant_data/learningTrialsLogData", 
                                  subject_num, "_", subject_city, ".csv", sep = ""), row.names = FALSE)

# Save recreate_paths_log to csv file
write.csv(recreate_paths_log, paste("E:/Nav Stress Data/Participant_data/recreatePathsLogData", 
                                   subject_num, "_", subject_city, ".csv", sep = ""), row.names = FALSE)






