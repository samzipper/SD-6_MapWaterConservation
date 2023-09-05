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

# plot timeseries
ggplot(df_long, aes(x = MonthYear)) +
  geom_line(aes(y = irrEst_mm, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm), color = "black") +
  facet_wrap(~FieldID, ncol = 1) +
  scale_color_manual(values = pal_algorithms)

# plot cumulative irrigation
ggplot(df_long_cumirr, aes(x = MonthYear)) +
  geom_line(aes(y = irrEst_mm_cumsum, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm_cumsum), color = "black") +
  facet_wrap(~FieldID, ncol = 1) +
  scale_color_manual(values = pal_algorithms)

# scatterplot comparison
ggplot(df_long, aes(x = irrEst_mm, y = irrigation_mm, color = FieldID)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  facet_wrap(~Algorithm)
