## DataPrep_00_CompareMetDataSources.R
# This script is intended to compare Google Earth Engine-derived gridMET data
# with Jill's precipitation field from SALUS output.

source(file.path("code", "paths+packages.R"))

## load data
# gridMET extracted from GEE by Sam
df_yearly_GEE <-
  read_csv(file.path("data", "gridmet_AnnualByField.csv"))

# gridMET extracted from GEE by Tom
df_yearly_fromTom <-
  read_csv(file.path(dir_data, "gridMET", "DataFromTom", "gridmet_AnnualByField.csv"))

# gridMET from SALUS
SALUS_raw <- 
  read_csv(file.path(dir_data, "SALUS", "SD6_SALUS_and_cropChoice2.csv"), col_types = "ddddddddcdcddcd")

## combine
df_combo <-
  SALUS_raw |>
  select(UID, year, PPT_ann_mm) |>
  rename(Year = year, precip_mm = PPT_ann_mm) |>
  left_join(df_yearly_GEE, by = c("UID", "Year"), suffix = c(".SALUS", ".GEE")) |>
  left_join(df_yearly_fromTom, by = c("UID", "Year"), suffix = c("", ".Tom")) |>
  select(-starts_with("ET")) |>
  rename(precip_mm.Tom = precip_mm)

# plot
ggplot(subset(df_combo, Year >= 2016), aes(x = precip_mm.SALUS, y = precip_mm.Tom, color = factor(Year))) +
  geom_point() +
  scale_color_viridis_d() +
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(subset(df_combo, Year >= 2016), aes(x = precip_mm.SALUS, y = precip_mm.GEE, color = factor(Year))) +
  geom_point() +
  scale_color_viridis_d() +
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(subset(df_combo, Year %in% c(2016, 2017)), aes(x = precip_mm.Tom, y = precip_mm.GEE, color = factor(Year))) +
  geom_point() +
  scale_color_viridis_d() +
  geom_abline(intercept = 0, slope = 1, color = "red")
