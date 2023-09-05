## FieldData_02_EstimateIrrigation.R
# This script is intended to estimate irrigation data from OpenET data.

source(file.path("code", "paths+packages.R"))

# folder where copmiled farmer data stored
dir_data <- "G:/.shortcut-targets-by-id/1fM3-4oKs6lEiTg-VQECNObDmlw9jVdX3/EGGS/NASA OpenET/data/field-specific water use"

# load all fields and remove data you don't want
df_all <- read_csv(file.path(dir_data, "FieldData_AllFieldsCompiled.csv")) |> 
  mutate(MonthYear = mdy(MonthYear)) |> 
  subset(!(year(MonthYear) == 2016 & FieldID == "SW1"))  # remove 2016 SW1 - no ET data

# pivot to long form
df_long <- 
  df_all |> 
  pivot_longer(c("ensemble", "disalexi", "ptjpl", "eemetric", "geesebal", "sims", "ssebop"),
               names_to = "Algorithm", values_to = "ET_mm") |> 
  arrange(FieldID, Algorithm)

# estimate irrigation as ET-P
df_long$ET.P_mm <- df_long$ET_mm - df_long$precip_mm
df_long$irrEst_mm <- ifelse(df_long$ET.P_mm >= 0, df_long$ET.P_mm, 0)

# calculate cumulative irrigation
df_long_cumirr <- 
  df_long |> 
  group_by(FieldID, Algorithm) |> 
  reframe(irrigation_mm_cumsum = cumsum(irrigation_mm),
          irrEst_mm_cumsum = cumsum(irrEst_mm)) |> 
  mutate(MonthYear = df_long$MonthYear)

## monthly plots

# plot timeseries
ggplot(df_long, aes(x = MonthYear)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line(aes(y = irrEst_mm/25.4, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm/25.4), color = "black", shape = 1) +
  facet_wrap(~FieldID, ncol = 1) +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  scale_x_date(name = "Month") +
  scale_y_continuous(name = "Monthly Irrigation [in]") +
  labs(title = "Monthly Estimated (color lines) and Reported (black dots) Irrigation Depths") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyTimeseries.png", width = 150, height = 150, units = "mm")

# plot cumulative irrigation
ggplot(df_long_cumirr, aes(x = MonthYear)) +
  geom_line(aes(y = irrEst_mm_cumsum/25.4, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm_cumsum/25.4), color = "black", shape = 1) +
  facet_wrap(~FieldID, ncol = 1) +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  scale_x_date(name = "Month") +
  scale_y_continuous(name = "Monthly Cumulative Irrigation [in]") +
  labs(title = "Monthly Estimated (color lines) and Reported (black dots) Cumulative Irrigation Depths") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyCumulativeTimeseries.png", width = 150, height = 150, units = "mm")

# scatterplot comparison
ggplot(df_long, aes(x = irrEst_mm/25.4, y = irrigation_mm/25.4, color = FieldID)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~Algorithm, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Monthly Irrigation [in]") +
  scale_y_continuous(name = "Reported Monthly Irrigation [in]") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyScatter.png", width = 150, height = 150, units = "mm")

## calculate annual irrigation
df_yr <-
  df_long |> 
  mutate(Year = year(MonthYear)) |> 
  group_by(FieldID, Year, Algorithm) |> 
  summarize(irrEst_mm_yr = sum(ET.P_mm),
            irrigation_mm_yr = sum(irrigation_mm))

ggplot(df_yr, aes(x = irrEst_mm_yr/25.4, y = irrigation_mm_yr/25.4, color = FieldID)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~Algorithm, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Annual Irrigation [in]") +
  scale_y_continuous(name = "Reported Annual Irrigation [in]") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_AnnualScatter.png", width = 150, height = 150, units = "mm")
