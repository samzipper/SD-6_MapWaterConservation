## FieldData_02_EstimateIrrigation.R
# This script is intended to estimate irrigation data from OpenET data.

source(file.path("code", "paths+packages.R"))

# folder where compiled farmer data stored
dir_farm_data <- "G:/My Drive/Projects-Active/NASA OpenET/data/field-specific water use"

# load all fields and remove data you don't want
df_all <- read_csv(file.path(dir_farm_data, "FieldData_AllFieldsCompiled-Monthly.csv"))

# pivot to long form
df_long <- 
  df_all |> 
  pivot_longer(ends_with("_et_mm"),
               names_to = "Algorithm", values_to = "ET_mm") |> 
  arrange(FieldID, Algorithm) |> 
  mutate(Algorithm = str_sub(Algorithm, start = 1, end = -7))

# estimate irrigation as ET-P
df_long$ET.P_mm <- df_long$ET_mm - df_long$precip_mm
df_long$irrEst_mm <- ifelse(df_long$ET.P_mm >= 0, df_long$ET.P_mm, 0)
df_long$P.Irr_mm <- df_long$precip_mm + df_long$irrigation_mm

# calculate cumulative irrigation
df_long_cumirr <- 
  df_long |> 
  group_by(FieldID, Algorithm) |> 
  reframe(irrigation_mm_cumsum = cumsum(irrigation_mm),
          irrEst_mm_cumsum = cumsum(irrEst_mm),
          P.Irr_mm_cumsum = cumsum(P.Irr_mm),
          ET_mm_cumsum = cumsum(ET_mm),
          ETo_mm_cumsum = cumsum(ETo_mm)) |> 
  mutate(MonthYear = df_long$MonthYear)

## calculate annual ET/P+I
df_long$Year <- year(df_long$MonthYear)
df_long$YearNov1 <- year(df_long$MonthYear + days(62))
df_long$WaterYear <- year(df_long$MonthYear + days(92))
names(df_long)
df_long <- df_long[,c(1,2,13,14,15, seq(3, 12))]
df_long_Year <-
  df_long |> 
  group_by(FieldID, Year, Algorithm) |> 
  summarize(ET_mm_sum = sum(ET_mm),
            irrigation_mm_sum = sum(irrigation_mm),
            precip_mm_sum = sum(precip_mm),
            ET_P.I = ET_mm_sum/(irrigation_mm_sum + precip_mm_sum))
df_long_Year$DeepPerc_mm_sum <- df_lm$lmSlope[df_lm$ts == "WaterYear"]*df_long_Year$precip_mm_sum + df_lm$lmInt[df_lm$ts == "WaterYear"]
df_long_Year$DeepPerc_mm_sum[df_long_Year$DeepPerc_mm_sum < 0] <- 0
df_long_Year$Peff_mm_sum <- df_long_Year$precip_mm_sum - df_long_Year$DeepPerc_mm_sum
df_long_Year$ET_Peff.I <- df_long_Year$ET_mm_sum/(df_long_Year$irrigation_mm_sum + df_long_Year$Peff_mm_sum)

ggplot(df_long_Year, aes(x = factor(Year), y = ET_P.I, fill = Algorithm)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  scale_fill_manual(values = pal_algorithms, labels = labs_algorithms) +
  labs(title = "Aggregated by Calendar Year")

# load deep percolation regressions
df_lm <- read_csv(file.path("data", "DeepPercRegressions_Summary.csv")) # DP = lmSlope*AnnualPrecip_mm + lmInt

df_long_YearNov1 <-
  df_long |> 
  subset(YearNov1 <= 2022) |> 
  group_by(FieldID, YearNov1, Algorithm) |> 
  summarize(ET_mm_sum = sum(ET_mm),
            irrigation_mm_sum = sum(irrigation_mm),
            precip_mm_sum = sum(precip_mm),
            ET_P.I = ET_mm_sum/(irrigation_mm_sum + precip_mm_sum))
df_long_YearNov1$DeepPerc_mm_sum <- df_lm$lmSlope[df_lm$ts == "WaterYear"]*df_long_YearNov1$precip_mm_sum + df_lm$lmInt[df_lm$ts == "WaterYear"]
df_long_YearNov1$DeepPerc_mm_sum[df_long_YearNov1$DeepPerc_mm_sum < 0] <- 0
df_long_YearNov1$Peff_mm_sum <- df_long_YearNov1$precip_mm_sum - df_long_YearNov1$DeepPerc_mm_sum
df_long_YearNov1$ET_Peff.I <- df_long_YearNov1$ET_mm_sum/(df_long_YearNov1$irrigation_mm_sum + df_long_YearNov1$Peff_mm_sum)

ggplot(df_long_YearNov1, aes(x = factor(YearNov1), y = ET_P.I, fill = Algorithm)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  scale_fill_manual(values = pal_algorithms, labels = labs_algorithms) +
  labs(title = "Aggregated by Nov 1 - Oct 31")


df_long_WaterYear <-
  df_long |> 
  subset(WaterYear <= 2022) |> 
  group_by(FieldID, WaterYear, Algorithm) |> 
  summarize(ET_mm_sum = sum(ET_mm),
            irrigation_mm_sum = sum(irrigation_mm),
            precip_mm_sum = sum(precip_mm),
            ET_P.I = ET_mm_sum/(irrigation_mm_sum + precip_mm_sum))
df_long_WaterYear$DeepPerc_mm_sum <- df_lm$lmSlope[df_lm$ts == "WaterYear"]*df_long_WaterYear$precip_mm_sum + df_lm$lmInt[df_lm$ts == "WaterYear"]
df_long_WaterYear$DeepPerc_mm_sum[df_long_WaterYear$DeepPerc_mm_sum < 0] <- 0
df_long_WaterYear$Peff_mm_sum <- df_long_WaterYear$precip_mm_sum - df_long_WaterYear$DeepPerc_mm_sum
df_long_WaterYear$ET_Peff.I <- df_long_WaterYear$ET_mm_sum/(df_long_WaterYear$irrigation_mm_sum + df_long_WaterYear$Peff_mm_sum)

ggplot(df_long_WaterYear, aes(x = factor(WaterYear), y = ET_P.I, fill = Algorithm)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  scale_fill_manual(values = pal_algorithms, labels = labs_algorithms) +
  labs(title = "Aggregated by Oct 1 - Sept 30")

# compare aggregation timescales, ensemble only
df_ens_Year <- subset(df_long_Year, Algorithm == "ensemble") |> 
  mutate(timescale = "Jan-Dec")
df_ens_WaterYear <- subset(df_long_WaterYear, Algorithm == "ensemble") |> 
  rename(Year = WaterYear) |> 
  mutate(timescale = "Oct-Sep")
df_ens_YearNov1 <- subset(df_long_YearNov1, Algorithm == "ensemble") |> 
  rename(Year = YearNov1) |> 
  mutate(timescale = "Nov-Oct")
df_ens_yrs <- bind_rows(df_ens_Year, df_ens_WaterYear, df_ens_YearNov1)

ggplot(df_ens_yrs, aes(x = factor(Year), y = ET_P.I, fill = timescale)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  labs(title = "Ensemble ET, comparing aggregation timescale")

ggplot(df_ens_yrs, aes(x = ET_P.I, fill = timescale)) +
  geom_vline(xintercept = 1, color = col.gray) +
  geom_histogram() +
  facet_wrap(~timescale, ncol = 1) +
  labs(title = "Ensemble ET, comparing aggregation timescale")

df_ens_yrs |> 
  group_by(timescale) |> 
  summarize(ET_P.I_mean = mean(ET_P.I),
            ET_P.I_median = median(ET_P.I),
            ET_P.I_stdev = sd(ET_P.I))

df_ens_yrs |> 
  subset(timescale == "Oct-Sep") |> 
  group_by(Year) |> 
  summarize(ET_P.I_mean = mean(ET_P.I),
            ET_P.I_median = median(ET_P.I),
            ET_P.I_stdev = sd(ET_P.I))

# compare fields
ggplot(df_ens_WaterYear, aes(x = FieldID, y = ET_P.I)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  labs(title = "Ensemble ET, Oct-Sep timescale")

ggplot(df_ens_WaterYear, aes(x = FieldID, y = factor(Year), fill = ET_P.I)) +
  geom_raster() +
  labs(title = "Ensemble ET, Oct-Sep timescale") +
  scale_fill_gradient2(midpoint = 1, mid = "gray95")

# verify math - match Forrest spreadsheet
subset(df_ens_WaterYear, FieldID == "SW1" & Year == 2017)$ET_P.I # should be 0.843
subset(df_ens_WaterYear, FieldID == "SW1" & Year == 2018)$ET_P.I # should be 0.974
subset(df_ens_WaterYear, FieldID == "SW1" & Year == 2019)$ET_P.I # should be 0.972
subset(df_ens_WaterYear, FieldID == "NB1" & Year == 2017)$ET_P.I # should be 0.864
subset(df_ens_WaterYear, FieldID == "NC1" & Year == 2022)$ET_P.I # should be 1.02



ggplot(df_ens_yrs, aes(x = factor(Year), y = ET_Peff.I, fill = timescale)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  labs(title = "Ensemble ET, comparing aggregation timescale", subtitle = "Effective precipitation")

df_ens_yrs |> 
  subset(timescale == "Oct-Sep") |> 
  group_by(Year) |> 
  summarize(ET_Peff.I_mean = mean(ET_Peff.I),
            ET_Peff.I_median = median(ET_Peff.I),
            ET_Peff.I_stdev = sd(ET_Peff.I))

ggplot(df_ens_yrs, aes(x = ET_Peff.I, fill = timescale)) +
  geom_vline(xintercept = 1, color = col.gray) +
  geom_histogram() +
  facet_wrap(~timescale, ncol = 1) +
  labs(title = "Ensemble ET, comparing aggregation timescale")

df_ens_yrs |> 
  group_by(timescale) |> 
  summarize(ET_Peff.I_mean = mean(ET_Peff.I),
            ET_Peff.I_median = median(ET_Peff.I),
            ET_Peff.I_stdev = sd(ET_Peff.I))

ggplot(df_ens_WaterYear, aes(x = FieldID, y = ET_Peff.I)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_boxplot() +
  labs(title = "Ensemble ET, Oct-Sep timescale")

ggplot(df_ens_WaterYear, aes(x = FieldID, y = factor(Year), fill = ET_Peff.I)) +
  geom_raster() +
  labs(title = "Ensemble ET, Oct-Sep timescale") +
  scale_fill_gradient2(midpoint = 1, mid = "gray95")

## monthly plots

# plot timeseries
ggplot(df_long, aes(x = MonthYear)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line(aes(y = irrEst_mm/25.4, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm/25.4), color = "black", shape = 1) +
  facet_wrap(~FieldID) +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  scale_x_date(name = "Month") +
  scale_y_continuous(name = "Monthly Irrigation [in]") +
  labs(title = "Monthly Estimated (color lines) and Reported (black dots) Irrigation Depths") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyTimeseries.png", width = 150, height = 150, units = "mm")

# plot timeseries including ETo
ggplot(df_long, aes(x = MonthYear)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line(aes(y = ET_mm/25.4, color = Algorithm)) +
  geom_line(aes(y = ETo_mm/25.4), color = "black", linetype = "dashed") +
  facet_wrap(~FieldID) +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  scale_x_date(name = "Month") +
  scale_y_continuous(name = "Monthly Flux [in]") +
  labs(title = "Monthly ET (color lines) and ETo (dashed line) Depths") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyTimeseries_ET+ETo.png", width = 150, height = 150, units = "mm")

# plot cumulative irrigation
ggplot(df_long_cumirr, aes(x = MonthYear)) +
  geom_line(aes(y = irrEst_mm_cumsum/25.4, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm_cumsum/25.4), color = "black", shape = 1) +
  facet_wrap(~FieldID, scales = "free_y") +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  scale_x_date(name = "Month") +
  scale_y_continuous(name = "Monthly Cumulative Irrigation [in]") +
  labs(title = "Monthly Estimated (color lines) and Reported (black dots) Cumulative Irrigation Depths") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyCumulativeTimeseries.png", width = 150, height = 150, units = "mm")

# plot cumulative P+Irrigation and ET
ggplot(df_long_cumirr, aes(x = MonthYear)) +
  geom_line(aes(y = ET_mm_cumsum/25.4, color = Algorithm)) +
  geom_point(aes(y = P.Irr_mm_cumsum/25.4), color = "black", shape = 1) +
  facet_wrap(~FieldID, scales = "free_y") +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  scale_x_date(name = "Month") +
  scale_y_continuous(name = "Monthly Cumulative Flux [in]") +
  labs(title = "Cumulative Monthly ET (color lines) and Precip + Reported Irr (black dots)") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyCumulativeP+Irr.png", width = 150, height = 150, units = "mm")

# scatterplot comparison
ggplot(df_long, aes(x = irrEst_mm/25.4, y = irrigation_mm/25.4, color = FieldID)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~Algorithm, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Monthly Irrigation [in]") +
  scale_y_continuous(name = "Reported Monthly Irrigation [in]") +
  theme(legend.position = "bottom")
ggsave("plots/FieldData_MonthlyScatter.png", width = 150, height = 150, units = "mm")