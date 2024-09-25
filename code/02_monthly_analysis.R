# Author: Siddharth Chaudhary
# Date: September 25, 2024
# Purpose: This script filters and analyzes sea ice extent data for SSP245 scenario, 
#          removing certain models, and visualizing time series for March. 
#          The plot includes observed data, a mean line across models, 
#          and a reference line at 1 million km².

library(ggplot2)
library(dplyr)

combined_data <- read.csv("/Users/sidchaudhary/Documents/GitHub/seaiceextent/data/combined_data.csv", header = TRUE)
combined_data$Date <- as.Date(paste(combined_data$Year, combined_data$Month, "01", sep = "-"))

## "ssp126" "ssp245" "ssp370" "ssp585"
combined_data <- combined_data[c(-1)]
combined_data_ssp <- filter(combined_data, Scenario == "ssp245")

filtered_combined_data <- combined_data_ssp %>%
  filter(Year > 1978)
obs_model <- rbind(filtered_combined_data, nsidc_obs)

# Remove "CIESM" and "CMCC-CM2-SR5" from the data and filter for Month 3 (March)
filtered_september_data <- obs_model %>%
  filter(Month == 3, 
         !Model %in% c("CIESM", "CMCC-CM2-SR5")) %>%  # Exclude the specified models
  mutate(Date = as.Date(Date))  # Ensure Date column is in Date format

# Calculate the mean value across all models except "Observed" for March
mean_september_data <- filtered_september_data %>%
  filter(Model != "Observed") %>%
  group_by(Date) %>%
  summarize(MeanValue = mean(Value, na.rm = TRUE))

# Plot the time series for March
ggplot() +
  # Time series for all models in March
  geom_line(data = filtered_september_data, aes(x = Date, y = Value, group = Model, color = Model), size = 0.3) +
  
  # Bold red line for "Observed" model
  geom_line(data = filtered_september_data %>% filter(Model == "Observed"), 
            aes(x = Date, y = Value), color = "red", size = 1.2) +
  
  # Bold black line for the mean across models in March
  geom_line(data = mean_september_data, aes(x = Date, y = MeanValue), color = "black", size = 1.2, linetype = "solid") +
  
  # Add a horizontal red dotted line at Value = 1
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +
  
  # Customize plot labels and theme
  labs(title = "Time Series of Sea Ice Extent in March - SSP245",
       x = "Date",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")),  # Custom Y-axis label
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "none")
