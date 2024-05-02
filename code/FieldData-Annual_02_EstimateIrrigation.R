## FieldData-Annual_02_EstimateIrrigation.R

source(file.path("code", "paths+packages.R"))

# folder where compiled farmer data stored
#setwd("C:/Users/s947z036/WorkGits/SD-6_MapWaterConservation")
dir_farm_data <- "G:/My Drive/Projects-Active/NASA OpenET/data/field-specific water use"

sf_fields <- st_read(file.path(dir_farm_data, "OpenET_FieldWaterUseBoundaries.shp"))
sf_fields$area_m2 <- as.numeric(st_area(st_make_valid(sf_fields)))

# load deep percolation regressions
df_lm <- read_csv(file.path("data", "DeepPercRegressions_Summary.csv")) # DP = lmSlope*AnnualPrecip_mm + lmInt

# loop through timesteps ("Annual", "WaterYear", "GrowingSeason")
ts_all <- c("Annual", "WaterYear", "GrowingSeason")
for (ts in ts_all){
  
  # load all fields
  df_all <- read_csv(file.path(dir_farm_data, paste0("FieldData_AllFieldsCompiled-", ts, ".csv")))
  
  # pivot to long form
  df_long <- 
    df_all |> 
    pivot_longer(ends_with("_et_mm"),
                 names_to = "Algorithm", values_to = "ET_mm") |> 
    arrange(FieldID, Algorithm) |> 
    mutate(Algorithm = str_sub(Algorithm, start = 1, end = -7),
           timescale = ts)
  
  # calculate effective precipitation
  df_long$DeepPerc_mm <- df_lm$lmSlope[df_lm$ts == ts]*df_long$precip_mm + df_lm$lmInt[df_lm$ts == ts]
  df_long$DeepPerc_mm[df_long$DeepPerc_mm < 0] <- 0
  df_long$Peff_mm <- df_long$precip_mm - df_long$DeepPerc_mm
  
  # estimate irrigation as ET-P
  df_long$ET.P_mm <- df_long$ET_mm - df_long$precip_mm
  df_long$ET.Peff_mm <- df_long$ET_mm - df_long$Peff_mm
  df_long$irrEst_mm <- ifelse(df_long$ET.P_mm >= 0, df_long$ET.P_mm, 0)
  df_long$irrEstPeff_mm <- ifelse(df_long$ET.Peff_mm >= 0, df_long$ET.Peff_mm, 0)
  df_long$P.Irr_mm <- df_long$precip_mm + df_long$irrigation_mm
  df_long$Peff.Irr_mm <- df_long$Peff_mm + df_long$irrigation_mm
  df_long$ET_P.I <- df_long$ET_mm/df_long$P.Irr_mm
  df_long$ET_Peff.I <- df_long$ET_mm/df_long$Peff.Irr_mm
  
  if (ts == ts_all[1]){
    df_long_all <- df_long
  } else {
    df_long_all <- bind_rows(df_long_all, df_long)
  }
}

## evaluate ET_P.I output
# boxplot - distribution by year and timescale
ggplot(subset(df_long_all, Algorithm == "ensemble"), aes(x = factor(Year), y= ET_P.I, fill = timescale)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_hline(yintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  geom_boxplot()

df_long_all |> 
  subset(Algorithm == "ensemble" &
           timescale == "WaterYear") |> 
  group_by(Year) |> 
  summarize(ET_P.I_mean = mean(ET_P.I),
            ET_P.I_median = median(ET_P.I),
            ET_P.I_stdev = sd(ET_P.I))

# histogram - distribution by timescale
ggplot(subset(df_long_all, Algorithm == "ensemble"), aes(x = ET_P.I, fill = timescale)) +
  geom_histogram() +
  geom_vline(xintercept = 1, color = col.gray) +
  geom_vline(xintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  facet_wrap(~timescale, ncol = 1)

df_long_all |> 
  subset(Algorithm == "ensemble") |> 
  group_by(timescale) |> 
  summarize(ET_P.I_mean = mean(ET_P.I),
            ET_P.I_median = median(ET_P.I),
            ET_P.I_stdev = sd(ET_P.I))

# boxplot - distribution by field and timescale
ggplot(subset(df_long_all, Algorithm == "ensemble"), aes(x = FieldID, y= ET_P.I, fill = timescale)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_hline(yintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  geom_boxplot()

# raster - distribution by field and year
ggplot(subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear"), 
       aes(x = FieldID, y = factor(Year), fill = ET_P.I)) +
  geom_raster() +
  labs(title = "Ensemble ET, WaterYear timescale") +
  scale_fill_gradient2(midpoint = 1, mid = "gray95")

## evaluate ET_Peff.I output
# boxplot - distribution by year and timescale
ggplot(subset(df_long_all, Algorithm == "ensemble"), aes(x = factor(Year), y= ET_Peff.I, fill = timescale)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_hline(yintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  geom_boxplot()

df_long_all |> 
  subset(Algorithm == "ensemble" &
           timescale == "WaterYear") |> 
  group_by(Year) |> 
  summarize(ET_Peff.I_mean = mean(ET_Peff.I),
            ET_Peff.I_median = median(ET_Peff.I),
            ET_Peff.I_stdev = sd(ET_Peff.I))

# histogram - distribution by timescale
ggplot(subset(df_long_all, Algorithm == "ensemble"), aes(x = ET_Peff.I, fill = timescale)) +
  geom_histogram() +
  geom_vline(xintercept = 1, color = col.gray) +
  geom_vline(xintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  facet_wrap(~timescale, ncol = 1)

df_long_all |> 
  subset(Algorithm == "ensemble") |> 
  group_by(timescale) |> 
  summarize(ET_Peff.I_mean = mean(ET_Peff.I),
            ET_Peff.I_median = median(ET_Peff.I),
            ET_Peff.I_stdev = sd(ET_Peff.I))

# boxplot - distribution by field and timescale
ggplot(subset(df_long_all, Algorithm == "ensemble"), aes(x = FieldID, y= ET_Peff.I, fill = timescale)) +
  geom_hline(yintercept = 1, color = col.gray) +
  geom_hline(yintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  geom_boxplot()

# raster - distribution by field and year
ggplot(subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear"), 
       aes(x = FieldID, y = factor(Year), fill = ET_Peff.I)) +
  geom_raster() +
  labs(title = "Ensemble ET, WaterYear timescale") +
  scale_fill_gradient2(midpoint = 1, mid = "gray95")

# histogram - distribution by year
ggplot(subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear"), 
       aes(x = ET_Peff.I)) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(xintercept = 1, color = col.gray) +
  geom_vline(xintercept = c(0.8, 1.2), color = col.gray, linetype = "dashed") +
  facet_wrap(~factor(Year), ncol = 4)
labs(title = "Ensemble ET, WaterYear timescale")

df_long_all |> 
  subset(Algorithm == "ensemble" & timescale == "WaterYear") |> 
  group_by(Year) |> 
  summarize(ET_Peff.I_mean = mean(ET_Peff.I),
            ET_Peff.I_median = median(ET_Peff.I),
            ET_Peff.I_stdev = sd(ET_Peff.I))

## data quality screening
# Ott et al: pumping/Net ET < 0.5 or > 1.5

## base on long-term average
# any fields with long-term ET_Peff more than 20% from 1?
df_long_avg <-
  df_long_all |> 
  subset(timescale == "WaterYear" & Algorithm == "ensemble") |> 
  group_by(timescale, FieldID, Algorithm) |> 
  summarize(ET_P.I_mean = mean(ET_P.I),
            ET_Peff.I_mean = mean(ET_Peff.I))

summary(df_long_avg)
df_long_avg[abs(df_long_avg$ET_Peff.I_mean - 1) > 0.2, ]

fields_keep <- df_long_avg$FieldID[abs(df_long_avg$ET_Peff.I_mean - 1) <= 0.2]

# subset anything with ET_Peff.I more than 20% from 1
ggplot(subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear"), 
       aes(x = irrEstPeff_mm, y = irrigation_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = FieldID %in% fields_keep)) +
  stat_smooth(method = "lm", color = "black") +
  stat_smooth(data = subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear" &
                              FieldID %in% fields_keep),
              method = "lm", color = "blue") +
  scale_x_continuous(name = "Estimated Irrigation [mm]") +
  scale_y_continuous(name = "Reported Irrigation [mm]") +
  labs(title = "ts = WaterYear, Algorithm = ensemble")

lm(irrigation_mm ~ irrEstPeff_mm, data = subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear")) |> summary()
lm(irrigation_mm ~ irrEstPeff_mm, data = subset(df_long_all, Algorithm == "ensemble" & timescale == "WaterYear" &
                                                  FieldID %in% fields_keep)) |> summary()

# remove individual years (due to flowmeter issue, etc.)
# match fig 7 in Ott et al
df_plot <- 
  df_long_all |> 
  mutate(Irr.IrrEst = irrigation_mm/irrEstPeff_mm) |> 
  subset(Algorithm == "ensemble" & timescale == "WaterYear") |> 
  mutate(Irr.IrrEst = irrigation_mm/irrEstPeff_mm,
         Irr.IrrEst_outlier = abs(Irr.IrrEst - 1) > 0.5)

ggplot(df_plot, aes(x = irrEstPeff_mm, y = irrigation_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = Irr.IrrEst_outlier)) +
  stat_smooth(method = "lm", color = "black") +
  stat_smooth(data = subset(df_plot, Algorithm == "ensemble" & 
                              timescale == "WaterYear" &
                              !Irr.IrrEst_outlier),
              method = "lm", color = "red") +
  scale_x_continuous(name = "Estimated Irrigation [mm]") +
  scale_y_continuous(name = "Reported Irrigation [mm]") +
  labs(title = "ts = WaterYear, Algorithm = ensemble")

lm(irrigation_mm ~ irrEstPeff_mm, data = df_plot) |> summary()
lm(irrigation_mm ~ irrEstPeff_mm, data = subset(df_plot, !Irr.IrrEst_outlier)) |> summary()

df_plot_noOutliers <- subset(df_plot, !Irr.IrrEst_outlier)
ggplot(df_plot_noOutliers, 
       aes(y = irrigation_mm, x = irrEstPeff_mm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  stat_smooth(method = "lm")

round(100*rmse(df_plot_noOutliers$irrEstPeff_mm, df_plot_noOutliers$irrigation_mm)/mean(df_plot_noOutliers$irrigation_mm), 2)
round(100*mae(df_plot_noOutliers$irrEstPeff_mm, df_plot_noOutliers$irrigation_mm)/mean(df_plot_noOutliers$irrigation_mm), 2)
summary(lm(irrigation_mm ~ irrEstPeff_mm, data = df_plot_noOutliers))

df_plot_noOutliers_withArea <- left_join(df_plot_noOutliers, st_drop_geometry(sf_fields), by = "FieldID")
df_plot_noOutliers_withArea$irrigation_m3million <- (df_plot_noOutliers_withArea$irrigation_mm/1000)*df_plot_noOutliers_withArea$area_m2/1e6
df_plot_noOutliers_withArea$irrEstPeff_m3million <- (df_plot_noOutliers_withArea$irrEstPeff_mm/1000)*df_plot_noOutliers_withArea$area_m2/1e6
ggplot(df_plot_noOutliers_withArea, 
       aes(y = irrigation_m3million, x = irrEstPeff_m3million)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  stat_smooth(method = "lm")

round(100*rmse(df_plot_noOutliers_withArea$irrEstPeff_m3million, df_plot_noOutliers_withArea$irrigation_m3million)/mean(df_plot_noOutliers_withArea$irrigation_m3million), 2)
round(100*mae(df_plot_noOutliers_withArea$irrEstPeff_m3million, df_plot_noOutliers_withArea$irrigation_m3million)/mean(df_plot_noOutliers_withArea$irrigation_m3million), 2)
summary(lm(irrigation_m3million ~ irrEstPeff_m3million, data = df_plot_noOutliers_withArea))

## plots
# scatterplot
df_corn <- subset(df_long, cropType %in% c("Corn", "Corn-Grain", "Corn-Corn"))
ggplot(df_corn, aes(x = irrEst_mm/25.4, y = irrigation_mm/25.4)) +
  geom_abline(color = col.gray) +
  geom_point(aes(color = str_sub(FieldID, 1, 2))) +
  #stat_smooth(method = "lm", color = "black", linetype = "dashed") +
  scale_x_continuous(name = "Estimated Irrigation [in]") +
  scale_y_continuous(name = "Reported Irrigation [in]") +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = col.cat.blu, 
                                "WC" = col.cat.yel, 
                                "NW" = col.cat.org,
                                "NB" = col.cat.grn,
                                "SW" = col.cat.red)) +
  facet_wrap(~Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms))
ggsave(file.path("plots", "FieldData-Annual_ScatterAnnual.png"),
       width = 190, height = 75, units = "mm")

# plot cumsum - first choose only sites with >= 4 yrs (50%) data
fields_multiyr <- 
  df_all |> 
  group_by(FieldID) |> 
  summarize(n_years = n()) |> 
  subset(n_years >= 4)

df_multiyr <- subset(df_long_cumirr, FieldID %in% fields_multiyr$FieldID)
ggplot(data = df_multiyr, aes(x = Year)) +
  geom_line(aes(y = irrEst_mm_cumsum/25.4, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm_cumsum/25.4), shape = 1, color = "black") +
  facet_wrap(~FieldID, nrow = 4) +
  scale_x_continuous(breaks = seq(2016, 2022, 3)) +
  scale_y_continuous(name = "Cumulative Irrigation [in]") +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  theme(legend.position = "bottom")
ggsave(file.path("plots", "FieldData-Annual_Cumulative.png"),
       width = 150, height = 210, units = "mm")

# average across cumsum period
df_multiyr_mean <-
  df_multiyr |> 
  group_by(FieldID, Algorithm) |> 
  filter(Year == max(Year)) |> 
  left_join(fields_multiyr, by = "FieldID") |> 
  mutate(irrigation_mm_mean = irrigation_mm_cumsum/n_years,
         irrEst_mm_mean = irrEst_mm_cumsum/n_years)

ggplot(data = df_multiyr_mean, aes(x = irrEst_mm_mean/25.4, y = irrigation_mm_mean/25.4)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(n_years))) +
  facet_wrap(~Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Estimated Irrigation [in/yr], Multi-Year Mean",
                     limits = c(0, 31)) +
  scale_y_continuous(name = "Reported Irrigation [in/yr], Multi-Year Mean",
                     limits = c(0, 31)) +
  coord_equal() +
  scale_color_manual(name = "# of Years", values = c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e"))
ggsave(file.path("plots", "FieldData-Annual_ScatterMean.png"),
       width = 190, height = 100, units = "mm")
