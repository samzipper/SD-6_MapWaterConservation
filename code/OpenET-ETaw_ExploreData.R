## OpenET-ETaw_ExploreData.R
# Explore ETaw data from OpenET.

source(file.path("code", "paths+packages.R"))

## load field attributes
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5)
fields_landcover <- 
  read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

fields_attributes <- 
  left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) |> 
  left_join(fields_spatial, by = "UID")

## load and process ETaw data
dir_ETaw <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/LEMA_Sheridan-6/data/OpenET/ETaw_20240604extraction"

#rename to match format of 'ET_Monthly_All_FieldsNoDups.csv' file, join with attributes
#  UID,Date,ETaw_mm_mean_ensemble
df_ETaw_mo <- 
  read_csv(file.path(dir_ETaw, "ETaw_mean_vals.csv")) |> 
  rename(Date = image_date, ETaw_mm_mean_ensemble = mean) |> 
  dplyr::select(-area) |> 
  mutate(Year = year(Date)) |> 
  left_join(fields_attributes, by = c("Year", "UID")) |> 
  subset(is.finite(ETaw_mm_mean_ensemble) & 
           is.finite(area_m2)) # no ETaw data for 2016, no attribute data for 2021

# aggregate to annual by field
df_ETaw_yr <-
  df_ETaw_mo |> 
  group_by(UID, Year, CropGroupCoarse, within_lema, area_m2, Irrigation) |> 
  summarize(ETaw_mm_ensemble_annual = sum(ETaw_mm_mean_ensemble))

## density plots - irrigated and dryland corn
subset(df_ETaw_yr, CropGroupCoarse == "Corn") |> 
  ggplot(aes(x = ETaw_mm_ensemble_annual, color = Irrigation)) +
  geom_density() +
  facet_wrap(~Year)

## LEMA total by year
df_ETaw_yr_LEMA <-
  df_ETaw_yr |> 
  subset(within_lema & Irrigation) |> 
  group_by(Year) |> 
  summarize(ETaw_m3_LEMAtotal = sum(area_m2*ETaw_mm_ensemble_annual/1000))

## load other results for comparison
# previous ET calculations
df_irr <- 
  read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_Annual.csv")) |> 
  subset(Algorithm == "ensemble") |> 
  dplyr::select(-Algorithm) |> 
  rename(ET.P_m3_LEMAtotal = OpenETirrigationLEMA_m3,
         ET.Peff_m3_LEMAtotal = OpenETirrigationLEMAPeff_m3)

# WIMAS data
df_wimas <- 
  read_csv(file.path("data", "WRGs_LEMAtotalIrrigation.csv")) |> 
  rename(Reported_m3_LEMAtotal = WIMASirrigationLEMA_m3) |> 
  subset(Year >= 2016 & Year <= 2020)

df_allLEMA <-
  df_irr |> 
  left_join(df_ETaw_yr_LEMA, by = "Year") |> 
  left_join(df_wimas, by = "Year") |> 
  pivot_longer(cols = ends_with("LEMAtotal"), 
               names_to = "Calculation",
               values_to = "LEMAtotal_m3")

ggplot(df_allLEMA, aes(x = Year, y = LEMAtotal_m3, color = Calculation)) +
  geom_line() +
  geom_point()

## compare field resolution data - ET-P, ET-Peff, ETaw
# load previous data
df_ET_yr <- 
  read_csv(file.path(dir_openet, "OpenET_FieldIrrigation_Annual.csv")) |> 
  group_by(UID, Year) |> 
  summarize(ET_mm_annual = sum(ET_mm),
            ET.P_mm_annual = sum(ET.P_mm),
            ET.Peff_mm_annual = sum(ET.Peff_mm))

# join with ETaw
df_ET.ETaw_yr <- left_join(df_ETaw_yr, df_ET_yr, by = c("UID", "Year"))

ggplot(subset(df_ET.ETaw_yr, CropGroupCoarse == "Corn" & within_lema),
       aes(x = ET_mm_annual, y = ETaw_mm_ensemble_annual, color = Irrigation)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~Year)

ggplot(subset(df_ET.ETaw_yr, CropGroupCoarse == "Corn" & within_lema),
       aes(x = ET.P_mm_annual, y = ETaw_mm_ensemble_annual, color = Irrigation)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~Year)

ggplot(subset(df_ET.ETaw_yr, CropGroupCoarse == "Corn" & within_lema),
       aes(x = ET.Peff_mm_annual, y = ETaw_mm_ensemble_annual, color = Irrigation)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~Year)

## look at weird 2017 results
df_ETaw_mo |> 
  subset(within_lema & CropGroupCoarse == "Corn") |> 
  ggplot(aes(x = factor(month(Date)), y = ETaw_mm_mean_ensemble)) +
  geom_boxplot() +
  facet_wrap(~year(Date)) +
  scale_x_discrete(name = "Month")

## try "bias-correcting" ETaw and ET-Peff based on median rainfed values
# ETaw bias correction
df_ETaw_yr_rainfedMedian <-
  df_ETaw_yr |> 
  subset(!Irrigation) |> 
  group_by(Year) |> 
  summarize(ETaw_mm_rainfed_median = median(ETaw_mm_ensemble_annual))

df_ETaw_yr_BiasCorr <-
  left_join(df_ETaw_yr, df_ETaw_yr_rainfedMedian, by = "Year") |> 
  mutate(ETaw_mm_BiasCorr = ETaw_mm_ensemble_annual - ETaw_mm_rainfed_median)
df_ETaw_yr_BiasCorr$ETaw_mm_BiasCorr[df_ETaw_yr_BiasCorr$ETaw_mm_BiasCorr < 0] <- 0

ggplot(df_ETaw_yr_BiasCorr, aes(x = ETaw_mm_BiasCorr, color = Irrigation)) +
  geom_density() +
  facet_wrap(~Year)

df_ETaw_yr_BiasCorr_LEMA <-
  df_ETaw_yr_BiasCorr |> 
  subset(within_lema & Irrigation) |> 
  group_by(Year) |> 
  summarize(ETaw_m3_BiasCorr_LEMAtotal = sum(area_m2*ETaw_mm_BiasCorr/1000))

# ET-Peff bias correction
df_ET.Peff_yr_rainfedMedian <-
  df_ET_yr |> 
  left_join(fields_attributes, by = c("UID", "Year")) |> 
  subset(!Irrigation) |> 
  group_by(Year) |> 
  summarize(ET.Peff_mm_rainfed_median = median(ET.Peff_mm_annual))

df_ET.Peff_yr_BiasCorr <-
  left_join(df_ET_yr, df_ET.Peff_yr_rainfedMedian, by = "Year") |> 
  left_join(fields_attributes, by = c("UID", "Year")) |> 
  mutate(ET.Peff_mm_BiasCorr = ET.Peff_mm_annual - ET.Peff_mm_rainfed_median)
df_ET.Peff_yr_BiasCorr$ET.Peff_mm_BiasCorr[df_ET.Peff_yr_BiasCorr$ET.Peff_mm_BiasCorr < 0] <- 0

ggplot(df_ET.Peff_yr_BiasCorr, aes(x = ET.Peff_mm_BiasCorr, color = Irrigation)) +
  geom_density() +
  facet_wrap(~Year)

df_ET.Peff_yr_BiasCorr_LEMA <-
  df_ET.Peff_yr_BiasCorr |> 
  subset(within_lema & Irrigation) |> 
  group_by(Year) |> 
  summarize(ET.Peff_m3_BiasCorr_LEMAtotal = sum(area_m2*ET.Peff_mm_BiasCorr/1000))

df_allLEMA_BiasCorr <-
  df_ET.Peff_yr_BiasCorr_LEMA |> 
  left_join(df_ETaw_yr_BiasCorr_LEMA, by = "Year") |> 
  left_join(df_wimas, by = "Year") |> 
  pivot_longer(cols = ends_with("LEMAtotal"), 
               names_to = "Calculation",
               values_to = "LEMAtotal_m3")

ggplot(df_allLEMA_BiasCorr, aes(x = Year, y = LEMAtotal_m3, color = Calculation)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"))
