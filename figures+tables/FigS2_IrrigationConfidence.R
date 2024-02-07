## IrrigationConfidence.R
# Investigate confidence in irrigation status.

source(file.path("code", "paths+packages.R"))

## choose data to use
ts <- "GrowingSeason"
alg <- "ensemble"

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))

## irrigation
# period to calculate/map
yr_start <- 2016
yr_end <- 2020 # no irrigation status for 2021 so cannot include

## load data
# field attributes
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv"))
fields_landcover <- 
  read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

# join attributes
fields_attributes <- 
  left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(Year >= yr_start & Year <= yr_end & within_lema)

fields_attributes$Irrigation <- fields_attributes$IrrigatedPrc > 0.5
fields_attributes$IrrConfidence <- "Unknown"
fields_attributes$IrrConfidence[fields_attributes$IrrigatedPrc <= 0.1 | fields_attributes$IrrigatedPrc >= 0.9] <- "High"

## get stats on irrigation confidence - from script "OpenET_03-EstimateIrrigation+Confidence.R
#df_irr <- read_csv(file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv"))) |> 
#  subset(Algorithm == alg)

## plot confidence by year
ggplot(fields_attributes, aes(x = factor(Year), fill = IrrConfidence)) + 
  geom_bar()

fields_attributes |> 
  mutate(IrrigatedPrc_bins = cut(IrrigatedPrc, breaks = seq(0, 1, 0.1), include.lowest = T)) |> 
  group_by(Year, IrrigatedPrc_bins) |> 
  summarize(BinAreaTotal_m2 = sum(area_m2)) |> 
  ggplot(aes(x = IrrigatedPrc_bins, y = BinAreaTotal_m2/10000, fill = IrrigatedPrc_bins)) + 
  geom_col() +
  facet_wrap(~Year, ncol = 1) +
  scale_x_discrete(name = "Irrigated Percentage within Field Boundary") +
  scale_y_continuous(name = "Total Area within SD-6 LEMA [ha]") +
  scale_fill_manual(values = c(rep(col.cat.yel, 5), rep(col.cat.grn, 5)),
                    guide = "none")


fields_attributes |> 
  mutate(IrrigatedPrc_bins = cut(IrrigatedPrc, breaks = c(0, 0.1, 0.5, 0.9, 1), include.lowest = T)) |> 
  group_by(Year, IrrigatedPrc_bins) |> 
  summarize(BinAreaTotal_m2 = sum(area_m2)) |> 
  ggplot(aes(x = IrrigatedPrc_bins, y = BinAreaTotal_m2/10000, fill = IrrigatedPrc_bins)) + 
  geom_col() +
  facet_wrap(~Year, ncol = 1) +
  scale_x_discrete(name = "Irrigated Proportion within Field Boundary") +
  scale_y_continuous(name = "Total Area within SD-6 LEMA [ha]") +
  scale_fill_manual(name = NULL, 
                    values = c(col.cat.yel, col.cat.org, col.cat.blu, col.cat.grn),
                    labels = c("High Confidence Rainfed (< 10% Irrigated)",
                               "Low Confidence Rainfed (10-50% Irrigated)",
                               "Low Confidence Irrigated (50-90% Irrigated)",
                               "High Confidence Irrigated (>90% Irrigated")) +
  guides(fill = guide_legend(nrow = 4)) +
  theme(legend.position = "bottom")
ggsave(file.path("figures+tables", "FigS2_IrrigationConfidence.png"),
       width = 95, height = 210, units = "mm")
