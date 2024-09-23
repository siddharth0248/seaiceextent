library(ggplot2)
library(dplyr)

filtered_combined_data <- combined_data %>%
  filter(Year > 1978)
obs_model <- rbind(filtered_combined_data,nsidc_obs)


# Remove "CIESM" and "CMCC-CM2-SR5" from the data and filter for Month 9 (September)
filtered_september_data <- obs_model %>%
  filter(Month == 9, 
         !Model %in% c("CIESM", "CMCC-CM2-SR5")) %>%  # Exclude the specified models
  mutate(Date = as.Date(Date))  # Ensure Date column is in Date format

# Calculate the mean value across all models except "Observed" for September
mean_september_data <- filtered_september_data %>%
  filter(Model != "Observed") %>%
  group_by(Date) %>%
  summarize(MeanValue = mean(Value, na.rm = TRUE))

# Plot the time series for September
ggplot() +
  # Time series for all models in September
  geom_line(data = filtered_september_data, aes(x = Date, y = Value, group = Model, color = Model), size = 0.3) +
  
  # Bold red line for "Observed" model
  geom_line(data = filtered_september_data %>% filter(Model == "Observed"), 
            aes(x = Date, y = Value), color = "red", size = 1.2) +
  
  # Bold black line for the mean across models in September
  geom_line(data = mean_september_data, aes(x = Date, y = MeanValue), color = "black", size = 1.2, linetype = "solid") +
  
  # Add a horizontal red dotted line at Value = 1
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +
  
  # Customize plot labels and theme
  labs(title = "Time Series of Sea Ice Extent in September - SSP585",
       x = "Date",
       y = expression(paste("Sea Ice Extent (", 10^6, " km"^2, ")")),  # Custom Y-axis label
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "none")
