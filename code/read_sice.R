library(tidyverse)
library(ggplot2)
library(dplyr)

# Directory containing your .txt files
folder_path <- "/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/model/"

# Function to read and process each file
read_and_process_file <- function(file_path) {
  # Read the file and extract its content
  file_content <- read_lines(file_path)
  
  # Extract Year, Month, and Value from the first line
  file_data <- read.table(text = file_content, header = FALSE, quote = "\"", comment.char = "", stringsAsFactors = FALSE)
  
  # Rename columns for clarity (optional)
  names(file_data) <- c("Year", "Month", "Value")
  
  # Extract file name and split it based on "_"
  file_name <- basename(file_path)
  file_name_parts <- str_split(file_name, "_")[[1]]
  
  # Create a dataframe with file name parts as separate columns
  file_info <- tibble::tibble(
    FileNamePart1 = file_name_parts[1],
    FileNamePart2 = file_name_parts[2],
    Model = file_name_parts[3],
    Time = file_name_parts[4],
    Scenario = file_name_parts[5],
    FileNamePart6 = file_name_parts[6],
    FileNamePart7 = file_name_parts[7]
  )
  
  # Return the combined data with the file name
  bind_cols(file_info, file_data)
}

# Get all .txt files in the folder
txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Read and process all files
combined_data <- map_dfr(txt_files, read_and_process_file)
combined_data <- combined_data[c(8,9,10,3,4,5)]
# View the combined dataframe
print(combined_data)

write.csv(combined_data,file = "/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/combined_data.csv")

combined_data <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/combined_data.csv", header = TRUE)

combined_data$Date <- as.Date(paste(combined_data$Year, combined_data$Month, "01", sep = "-"))
combined_data <- filter(combined_data,Scenario=="ssp245")

filtered_data <- combined_data %>%
  filter(Date >= as.Date("1979-01-01") & Date <= as.Date("2023-12-31"))
filtered_data <- filtered_data[c(-1)]

summary_data <- filtered_data %>%
  group_by(Year, Model) %>%
  summarise(Max_Value = max(Value),
            Min_Value = min(Value))

sea_ice_extent <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/nsidc_min_max.csv")

filtered_data <- rbind(filtered_data,nsidc_obs)


summary_data <- rbind(sea_ice_extent,summary_data)
summary_data$Model <- factor(summary_data$Model, levels = c(c("Observed", "ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", 
                                                              "CAMS-CSM1-0", "CanESM5", "CanESM5-CanOE", "CESM2", 
                                                              "CESM2-WACCM", "CIESM", "CMCC-CM2-SR5", "CNRM-CM6-1","CNRM-ESM2-1", 
                                                              "CNRM-CM6-1-HR", "E3SM-1-1", "EC-Earth3", "EC-Earth3-Veg", 
                                                              "EC-Earth3-Veg-LR", "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
                                                              "GFDL-CM4", "GFDL-ESM4", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", 
                                                              "INM-CM5-0", "INM-CM4-8","IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", 
                                                              "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3", 
                                                              "NorESM2-LM", "NorESM2-MM", "TaiESM1","UKESM1-0-LL")))

filtered_data$Model <- factor(filtered_data$Model, levels = levels(summary_data$Model))

# Plot using ggplot (1500*1000)
ggplot(filtered_data, aes(x = Date, y = Value)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), color = "red") +
  labs(title = "Time Series of Sea Ice Extent - SSP585",
       x = "Year",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")))+  # Custom Y-axis label +
  facet_wrap(~ Model)# Faceting by Model
  
ggplot(summary_data, aes(x = Year)) +
  geom_line(aes(y = Max_Value, color = "Max Value"), linetype = "solid") +
  geom_line(aes(y = Min_Value, color = "Min Value"), linetype = "solid") +
  labs(title = "Time Series of Sea Ice Extent - SSP585",
       x = "Year",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")),  # Custom Y-axis label
       color = "Legend") +
  facet_wrap(~ Model) +
  theme(
    text = element_text(size = 12, family = "sans"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

## Read NSIDC Data
original_nsidc_obs <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/nsidc_obs_v30.csv", header = TRUE)
column_names <- c("Year", "1", "2", "3", "4", "5", "6", 
                  "7", "8", "9", "10", "11", "12")
colnames(original_nsidc_obs) <- column_names

nsidc_obs <- gather(original_data, Month, Value, -Year)

# Add columns Model, Time, Scenario, Date
nsidc_obs$Model <- "Observed"
nsidc_obs$Time <- "historical"
nsidc_obs$Scenario <- "Observed"
nsidc_obs$Date <- paste(nsidc_obs$Year, nsidc_obs$Month, "01", sep = "-")
nsidc_obs$Date <- as.Date(nsidc_obs$Date)

# Remove unnecessary columns
nsidc_obs <- nsidc_obs[, c("Year", "Month", "Value", "Model", "Time", "Scenario", "Date")]

filtered_data <- rbind(filtered_data,nsidc_obs)

std_deviation <- filtered_data %>%
  group_by(Model) %>%
  summarise(std_dev = sd(Value, na.rm = TRUE))

print(std_deviation)

quantiles <- filtered_data %>%
  group_by(Model) %>%
  summarise(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    std_dev = sd(Value, na.rm = TRUE)
  )

# Merge quantiles with the original data
df_merged <- merge(filtered_data, quantiles, by = "Model")
df_merged <- df_merged %>%
  filter(is.finite(Value) & is.finite(Q1) & is.finite(median) & is.finite(Q3) & is.finite(std_dev))

df_merged$Model <- factor(df_merged$Model, levels = c(c("Observed", "ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", 
                                                        "CAMS-CSM1-0", "CanESM5", "CanESM5-CanOE", "CESM2", 
                                                        "CESM2-WACCM", "CIESM", "CMCC-CM2-SR5", "CNRM-CM6-1","CNRM-ESM2-1", 
                                                        "CNRM-CM6-1-HR", "E3SM-1-1", "EC-Earth3", "EC-Earth3-Veg", 
                                                        "EC-Earth3-Veg-LR", "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
                                                        "GFDL-CM4", "GFDL-ESM4", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", 
                                                        "INM-CM5-0", "INM-CM4-8","IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", 
                                                        "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3", 
                                                        "NorESM2-LM", "NorESM2-MM", "TaiESM1","UKESM1-0-LL")))


# Plot boxplot with quantiles marked
ggplot(df_merged, aes(x = Model, y = Value)) +
  geom_boxplot() +
  geom_point(aes(y = Q1, color = "Q1"), position = position_dodge(width = 0.75), size = 3) +
  geom_point(aes(y = median, color = "Median"), position = position_dodge(width = 0.75), size = 3) +
  geom_point(aes(y = Q3, color = "Q3"), position = position_dodge(width = 0.75), size = 3) +
  geom_point(aes(y = std_dev, color = "Std Dev"), position = position_dodge(width = 0.75), size = 3) +
  labs(title = "Distribution of Model Values with Quantiles and Standard Deviation - SSP245",
       x = "Model",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")),  # Custom Y-axis label
       color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Q1" = "red", "Median" = "blue", "Q3" = "green", "Std Dev" = "purple"),
                     breaks = c("Q1", "Median", "Q3", "Std Dev"),
                     labels = c("Q1", "Median", "Q3", "Std Dev"))
