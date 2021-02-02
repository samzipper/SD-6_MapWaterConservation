## Map_SD6+BufferBoundaries.R
# Map of LEMA and surroundings.

source(file.path("src", "paths+packages.R"))
library(ggspatial)

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries
sf_fields <- sf::st_read(file.path(dir_GIS, "landUse+irrigation", "CLU_SPLIT_ZM_inside_SD6buf10mi_CDL_2008_2018.shp"))

# points of diversion
df_wells <- read_csv(file.path("data", "water_right_groupings_sd6.csv"))
sf_wells <- sf::st_as_sf(x = df_wells, 
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = "+proj=longlat +datum=WGS84")

# figure out extent
extent <- sf::st_bbox(sf_buff)

ggplot(sf_buff) +
  geom_sf(data = sf_fields, aes(fill = factor(irr2018))) +
  geom_sf(color = "red", fill = NA, size = 2) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 2) +
  geom_sf(data = sf_wells, shape = "+") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  annotation_scale(location = "br", width_hint = 0.5, text_col = "white") +
  scale_fill_manual(name = "Irrigation Status [2018]",
                    values = c("0" = "gray65", "1" = "forestgreen"),
                    labels = c("0" = "Non-Irrigated", "1" = "Irrigated")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  ggsave(file.path("figures+tables", "Map_SD6+BufferBoundaries.png"), width = 6, height = 4, units = "in") +
  NULL
