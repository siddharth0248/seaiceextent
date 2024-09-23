##Plot for CMIP5
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

sie_hist_1979.2023 <- sie_hist_1979.2023 %>%
  mutate(Date = make_date(Year, Month, 1))

sie_hist_1979.2023 <- sie_hist_1979.2023 %>%
  mutate(across(3:10, ~ . / 10^6))

# Reshape the dataframe to long format
sie_hist_long <- sie_hist_1979.2023 %>%
  pivot_longer(cols = c(ACCESS10, ACCESS13, CCSM4, CESM1.CAM5, MIROC.ESM, MIROC.ESM.CHEM, MPI.ESM.LR, MPI.ESM.MR), 
               names_to = "Model", 
               values_to = "Value")


# Plot using ggplot (1500*1000)
ggplot(sie_hist_long, aes(x = Date, y = Value)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), color = "red") +
  labs(x = "Date (Year-Month)", y = "Value") +
  facet_wrap(~ Model, scales = "free_y") +
  scale_y_continuous(limits = c(0, 20)) +
  theme_minimal()


