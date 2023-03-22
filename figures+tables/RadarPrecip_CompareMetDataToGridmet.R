## RadarPrecip_CompareMetDataToGridmet.R

source(file.path("code", "paths+packages.R"))

# load data
df_yr_radar <- 
  read_csv(file.path("data", "RadarPrecip_GrowingSeasonByField.csv")) |> 
  dplyr::select(UID, Year, precip_mm)
df_yr_gridmet <- 
  read_csv(file.path("data", "gridmet_GrowingSeasonByField.csv")) |> 
  dplyr::select(UID, Year, precip_mm)
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv")) |> 
  subset(within_lema)

# join and subset to only fields in lema
df_met <- 
  left_join(df_yr_radar, df_yr_gridmet, by = c("UID", "Year"), suffix = c("_radar", "_gridmet")) |> 
  subset(UID %in% fields_spatial$UID)

# plot
ggplot(df_met, aes(x = precip_mm_gridmet, y = precip_mm_radar)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm") +
  facet_wrap(~Year, scales = "free") +
  scale_x_continuous(name = "Annual gridMET Precipitation [mm]") +
  scale_y_continuous(name = "Annual Radar Precipitation [mm]")

ggsave(file.path("figures+tables", "RadarPrecip_CompareMetDataToGridmet.png"),
       width = 190, height = 120, units = "mm")
