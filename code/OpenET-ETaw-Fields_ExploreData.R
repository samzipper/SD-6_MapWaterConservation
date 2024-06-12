## OpenET-ETaw-Fields_ExploreData.R
# Explore ETaw data from OpenET.

source(file.path("code", "paths+packages.R"))

## load and process ETaw data
dir_ETaw <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/LEMA_Sheridan-6/data/OpenET/ETaw_20240604extraction"

#rename to match format of 'ET_Monthly_All_FieldsNoDups.csv' file, join with attributes
#  UID,Date,ETaw_mm_mean_ensemble
df_ETaw_mo <- 
  read_csv(file.path(dir_ETaw, "IndividualFields_ETaw_monthly_sum_mean_vals_subset.csv")) |> 
  rename(Date = image_date) |> 
  mutate(Year = year(Date),
         ETaw_mm_mean_ensemble = mean*days_in_month(Date)) |> 
  dplyr::select(-`system:index`, -`.geo`, -mean)

# aggregate to annual by field
df_ETaw_yr <-
  df_ETaw_mo |> 
  group_by(fid, Year) |> 
  summarize(ETaw_mm_ensemble_annual = sum(ETaw_mm_mean_ensemble))

## load reported field data
# folder where compiled farmer data stored
dir_farm_data <- "G:/My Drive/Projects-Active/NASA OpenET/data/field-specific water use"
df_all <- read_csv(file.path(dir_farm_data, "FieldData_AllFieldsCompiled-Annual.csv"))

# shapefile to switch from fid to FieldID
sf_fields <- st_read(file.path(dir_farm_data, "OpenET_FieldWaterUseBoundaries.shp"))
sf_fields$area_m2 <- as.numeric(st_area(st_make_valid(sf_fields)))

## combine
df_ETaw_yr.withReported <-
  left_join(df_ETaw_yr, st_drop_geometry(sf_fields), by = "fid") |> 
  dplyr::select(FieldID, Year, ETaw_mm_ensemble_annual) |> 
  left_join(df_all, by = c("Year", "FieldID"))

## plots
# annual boxplot
ggplot(df_ETaw_yr.withReported, aes(x = Year, group = Year, y = ETaw_mm_ensemble_annual)) +
  geom_boxplot()

# monthly boxplor
ggplot(df_ETaw_mo, aes(x = Date, group = Date, y = ETaw_mm_mean_ensemble)) +
  geom_boxplot()

## plots - remove 2016 and 2022
df_ETaw_yr.plot <-
  df_ETaw_yr.withReported |> 
  subset(Year > 2016 & Year < 2022) |> 
  dplyr::select(Year, irrigation_mm, ETaw_mm_ensemble_annual, FieldID) |> 
  subset(is.finite(irrigation_mm))

# scatterplot
ggplot(df_ETaw_yr.plot, aes(x = irrigation_mm, y = ETaw_mm_ensemble_annual)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(title = "Annual ETaw vs. Reported Irrigation",
       subtitle = "No 2016 or 2022 data")

### get ET-Peff calculations in there too
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

df_long_withETaw <- 
  df_long_all |> 
  subset(timescale == "Annual" & Algorithm == "ensemble") |> 
  left_join(st_drop_geometry(sf_fields), by = "FieldID") |> 
  left_join(df_ETaw_yr, by = c("fid", "Year")) |> 
  subset(Year > 2016 & Year < 2022)

## compare ETaw to ET-Peff
ggplot(df_long_withETaw, aes(x = ET.Peff_mm, y = ETaw_mm_ensemble_annual)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_continuous(limits = c(0, 750)) +
  scale_y_continuous(limits = c(0, 750)) +
  labs(title = "Annual ETaw vs. ET-Peff from local regression",
       subtitle = "No 2016 or 2022 data")

ggplot(df_long_withETaw, aes(x = factor(Year), y = ETaw_mm_ensemble_annual)) +
  geom_boxplot() +
  labs(title = "Annual ETaw from all fields",
       subtitle = "No 2016 or 2022 data")

ggplot(df_long_withETaw, aes(x = ET_mm, y = ETaw_mm_ensemble_annual)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  scale_x_continuous(limits = c(200, 1200)) +
  scale_y_continuous(limits = c(200, 1200)) +
  labs(title = "Annual ETaw vs. ET",
       subtitle = "No 2016 or 2022 data")


ggplot(df_long_withETaw, aes(x = precip_mm, y = ETaw_mm_ensemble_annual)) +
  #geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  #scale_x_continuous(limits = c(200, 1200)) +
  #scale_y_continuous(limits = c(200, 1200)) +
  labs(title = "Annual ETaw vs. Precip",
       subtitle = "No 2016 or 2022 data")
