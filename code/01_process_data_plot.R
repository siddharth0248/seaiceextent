# Author: Siddharth Chaudhary
# Date: September 25, 2024
# Purpose: This script processes sea ice extent data from various models,
#          combines it with observed data, and performs time series analysis.
#          It also generates visualizations and calculates key statistics
#          such as standard deviation and quantiles for sea ice extent.

# Load required libraries
library(tidyverse)  # For data manipulation and plotting
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# Directory containing your .txt files (raw data)
folder_path <- "/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/model/"

# Function to read and process each file
read_and_process_file <- function(file_path) {
  # Read the file line by line
  file_content <- read_lines(file_path)
  
  # Extract data from the first line into a table format (Year, Month, and Value)
  file_data <- read.table(text = file_content, header = FALSE, quote = "\"", comment.char = "", stringsAsFactors = FALSE)
  
  # Rename the columns for better understanding
  names(file_data) <- c("Year", "Month", "Value")
  
  # Extract the file name and split it into components
  file_name <- basename(file_path)
  file_name_parts <- str_split(file_name, "_")[[1]]
  
  # Create a tibble with the file name components
  file_info <- tibble::tibble(
    FileNamePart1 = file_name_parts[1],
    FileNamePart2 = file_name_parts[2],
    Model = file_name_parts[3],      # Model name
    Time = file_name_parts[4],       # Time frame
    Scenario = file_name_parts[5],   # Scenario
    FileNamePart6 = file_name_parts[6],
    FileNamePart7 = file_name_parts[7]
  )
  
  # Combine the extracted file info and data content into one dataframe
  bind_cols(file_info, file_data)
}

# Get the list of all .txt files in the specified folder
txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Read and process all files, and combine them into a single dataframe
combined_data <- map_dfr(txt_files, read_and_process_file)
combined_data <- combined_data[c(8, 9, 10, 3, 4, 5)]  # Reordering columns for clarity
print(combined_data)  # Print the combined dataframe

# Save the combined data to a CSV file
write.csv(combined_data, file = "/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/combined_data.csv")

# Read the combined data from the saved CSV
combined_data <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/combined_data.csv", header = TRUE)

# Create a Date column by combining Year, Month, and setting a default day (01)
combined_data$Date <- as.Date(paste(combined_data$Year, combined_data$Month, "01", sep = "-"))

# Filter the data for a specific scenario ("ssp245")
combined_data <- filter(combined_data, Scenario == "ssp245")

# Remove unwanted columns (e.g., the first column)
filtered_data <- combined_data %>% select(-1)

# Summarize the data by Year and Model, calculating max and min values for each group
summary_data <- filtered_data %>%
  group_by(Year, Model) %>%
  summarise(Max_Value = max(Value), Min_Value = min(Value))

# Read sea ice extent observation data
sea_ice_extent <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/nsidc_min_max.csv")

# Combine observation data with the filtered model data
filtered_data <- rbind(filtered_data, nsidc_obs)
summary_data <- rbind(sea_ice_extent, summary_data)

# Define a factor for Model with specific levels (for consistent ordering in plots)
summary_data$Model <- factor(summary_data$Model, levels = c("Observed", "ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", 
                                                            "CAMS-CSM1-0", "CanESM5", "CanESM5-CanOE", "CESM2", 
                                                            "CESM2-WACCM", "CIESM", "CMCC-CM2-SR5", "CNRM-CM6-1", 
                                                            "CNRM-ESM2-1", "CNRM-CM6-1-HR", "E3SM-1-1", "EC-Earth3", 
                                                            "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FGOALS-f3-L", 
                                                            "FGOALS-g3", "FIO-ESM-2-0", "GFDL-CM4", "GFDL-ESM4", 
                                                            "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "INM-CM5-0", 
                                                            "INM-CM4-8", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", 
                                                            "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", 
                                                            "NESM3", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL"))

filtered_data$Model <- factor(filtered_data$Model, levels = levels(summary_data$Model))

# Plot the time series for Sea Ice Extent, faceted by Model
ggplot(filtered_data, aes(x = Date, y = Value)) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), color = "red") +
  labs(title = "Time Series of Sea Ice Extent - SSP245",
       x = "Year",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")"))) +  # Custom Y-axis label
  facet_wrap(~ Model)

# Plot summary of min/max Sea Ice Extent values for each model
ggplot(summary_data, aes(x = Year)) +
  geom_line(aes(y = Max_Value, color = "Max Value")) +  # Plot max values
  geom_line(aes(y = Min_Value, color = "Min Value")) +  # Plot min values
  labs(title = "Time Series of Sea Ice Extent - SSP245",
       x = "Year",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")),  # Custom Y-axis label
       color = "Legend") +
  facet_wrap(~ Model) +
  theme(text = element_text(size = 12, family = "sans"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))

# Load and clean NSIDC observation data
original_nsidc_obs <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/nsidc_obs_v30.csv", header = TRUE)

# Assign meaningful column names
colnames(original_nsidc_obs) <- c("Year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Reshape the data from wide format to long format
nsidc_obs <- gather(original_nsidc_obs, Month, Value, -Year)

# Add necessary columns for consistency with other datasets
nsidc_obs$Model <- "Observed"
nsidc_obs$Time <- "historical"
nsidc_obs$Scenario <- "Observed"
nsidc_obs$Date <- as.Date(paste(nsidc_obs$Year, nsidc_obs$Month, "01", sep = "-"))

# Keep only relevant columns
nsidc_obs <- nsidc_obs[, c("Year", "Month", "Value", "Model", "Time", "Scenario", "Date")]

# Combine observation data with filtered model data
filtered_data <- rbind(filtered_data, nsidc_obs)

# Calculate standard deviation for each model
std_deviation <- filtered_data %>%
  group_by(Model) %>%
  summarise(std_dev = sd(Value, na.rm = TRUE))

print(std_deviation)  # Print standard deviation for each model

# Calculate quantiles for each model
quantiles <- filtered_data %>%
  group_by(Model) %>%
  summarise(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    std_dev = sd(Value, na.rm = TRUE)
  )

# Merge quantile data back with original data
df_merged <- merge(filtered_data, quantiles, by = "Model")

# Filter out any non-finite values
df_merged <- df_merged %>%
  filter(is.finite(Value) & is.finite(Q1) & is.finite(median) & is.finite(Q3) & is.finite(std_dev))

# Plot boxplots with quantiles and standard deviation
ggplot(df_merged, aes(x = Model, y = Value)) +
  geom_boxplot() +
  geom_point(aes(y = std_dev), color = "red", size = 3) +
  labs(title = "Boxplots of Sea Ice Extent by Model",
       x = "Model",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")))
