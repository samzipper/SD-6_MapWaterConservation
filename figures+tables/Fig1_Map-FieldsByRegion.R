## Map-FieldsByRegion.R
# Create a map of the number of fields within each climate/ag division in Kansas.
#  Regions by number: https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/regional_monitoring/CLIM_DIVS/kansas.gif
#  Regions by geography: https://www.researchgate.net/publication/29867655/figure/fig4/AS:650032302792722@1531991186327/Kansas-Agricultural-Statistics-Districts-Map.png

source(file.path("code", "paths+packages.R"))

# load state and field boundaries
sf_divisions <- 
  st_read("C:/Users/s947z036/OneDrive - University of Kansas/GIS_GeneralFiles/AdministrativeBoundaries/US_AgReportingRegions/us48_asdfips_15sep14.shp") |> 
  subset(STATE == "KS")
sf_state <- st_read(file.path("data", "Tiger2010_Census_State.shp"))
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_hpa <- st_read(file.path("data", "High_Plains_Aquifer_Extent.shp"))

# add region character to ag divisions
sf_divisions$Region <- c("NW", "WC", "SW", "NC", "C", "SC", "NE", "EC", "SE")

# get number of fields in each region
dir_farm_data <- "G:/.shortcut-targets-by-id/1fM3-4oKs6lEiTg-VQECNObDmlw9jVdX3/EGGS/NASA OpenET/data/field-specific water use"
df_all <- read_csv(file.path(dir_farm_data, "FieldData_AllFieldsCompiled-Annual.csv")) |> 
  mutate(Region = str_sub(FieldID, 1, 2))
df_countByRegion <- df_all |> 
  group_by(Region) |> 
  summarize(FieldYears = n())

# create labels
sf_divLabels <-
  sf_divisions |> 
  subset(Region %in% df_countByRegion$Region) |> 
  left_join(df_countByRegion, by = "Region")
sf_divLabels$LabelLong <- paste0(sf_divLabels$Region, "\n(", sf_divLabels$FieldYears, " field-years)")
sf_divLabels$LabelShort <- paste0(sf_divLabels$Region, " (", sf_divLabels$FieldYears, ")")

# plot
ggplot() +
  geom_sf(data = sf_hpa, color = NA, fill = col.gray) +
  geom_sf(data = sf_divisions, color = "red", fill = NA) +
  geom_sf_text(data = sf_divLabels, aes(label = LabelShort)) +
  geom_sf(data = sf_lema, fill = col.cat.blu, color = col.cat.blu) +
  scale_x_continuous(breaks = seq(-101, -95, 2)) +
  scale_y_continuous(breaks = seq(37, 40, 1)) +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
ggsave(file.path("figures+tables", "Fig1_Map-FieldsByRegion.png"),
       width = 95, height = 85, units = "mm")
