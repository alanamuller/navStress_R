# Script to combine csv files output from navStress_unityData_processing_script.R

# Set working directory
setwd("E:/Nav Stress Data/Participant_data/")

# Define the pattern for the files you want to combine

data_pattern <- "navTrialsLogData"
#data_pattern <- "learningTrialsLogData"
data_pattern <- "recreatePathsLogData"

# List all csv files starting with "navTrialsLogData" in the directory
csv_files <- list.files(pattern = paste0("^", data_pattern, ".*\\.csv$"))

# Create an empty dataframe to store the combined data
combined_data <- data.frame()

# Loop through each csv file
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file, header = TRUE)
  
  # Combine the data
  combined_data <- rbind(combined_data, data)
}

# Print the combined data
print(combined_data)

# Write combined_data to a new csv file
write.csv(combined_data, paste0("E:/Nav Stress Data/combined_", data_pattern, ".csv"), row.names = FALSE)

