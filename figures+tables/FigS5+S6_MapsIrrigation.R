## FigS5+S6_MapsIrrigation.R

source(file.path("code", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))
irr_fields <- readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) # 1984-2020
lc_fields  <- readr::read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv"))  # 2006-2020

# estimated irrigation
df_irr <- read_csv(file.path(dir_openet, "OpenET_FieldIrrigation_GrowingSeason.csv"))

# join with sf
sf_irr <- full_join(sf_fields, df_irr, by = "UID")

# figure out extent
# if you want to include buffer use sf_buff; if just lema, use sf_lema here
bound <- sf::st_buffer(st_transform(sf_lema, st_crs(sf_irr)), dist = units::set_units(8000, "m"))
extent <- sf::st_bbox(st_transform(sf_lema, st_crs(sf_irr)))
sf_subset <- sf_irr[bound, op = st_intersects]

## map
p_et.P <-
  ggplot() +
  geom_sf(data = sf_subset, aes(fill = ET.P_mm), color = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"])) +
  facet_grid(Algorithm ~ Year,
             labeller = as_labeller(c(labs_algorithms, 
                                      "2016" = "2016", "2017" = "2017", "2018" = "2018", 
                                      "2019" = "2019", "2020" = "2020"))) +
  scale_fill_gradient2(name = "Growing Season ET - P [mm]",
                       breaks = seq(-300, 900, 300)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "FigS5_Map_ET.P.png"),
       p_et.P, width = 190, height = 195, units = "mm")

p_et.Peff <-
  ggplot() +
  geom_sf(data = sf_subset, aes(fill = ET.Peff_mm), color = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"])) +
  facet_grid(Algorithm ~ Year,
             labeller = as_labeller(c(labs_algorithms, 
                                      "2016" = "2016", "2017" = "2017", "2018" = "2018", 
                                      "2019" = "2019", "2020" = "2020"))) +
  scale_fill_gradient2(name = "Growing Season ET - Effective P [mm]") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "FigS5_Map_ET.Peff.png"),
       p_et.Peff, width = 190, height = 195, units = "mm")

p_irrDepth <- 
  ggplot() +
  geom_sf(data = subset(sf_subset, Irrigation), aes(fill = FieldIrrigation_mm), color = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  facet_grid(Algorithm ~ Year,
             labeller = as_labeller(c(labs_algorithms, 
                                      "2016" = "2016", "2017" = "2017", "2018" = "2018", 
                                      "2019" = "2019", "2020" = "2020"))) +
  scale_fill_viridis_c(name = "Irrigation Depth [mm]", direction = -1, option = "C") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "FigS6_Map_IrrDepth.png"),
       p_irrDepth, width = 190, height = 195, units = "mm")

p_irrDepth_Peff <- 
  ggplot() +
  geom_sf(data = subset(sf_subset, Irrigation), aes(fill = FieldIrrigationPeff_mm), color = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  facet_grid(Algorithm ~ Year,
             labeller = as_labeller(c(labs_algorithms, 
                                      "2016" = "2016", "2017" = "2017", "2018" = "2018", 
                                      "2019" = "2019", "2020" = "2020"))) +
  scale_fill_viridis_c(name = "Irrigation Depth [mm]", direction = -1, option = "C") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "FigS6_Map_IrrDepth_Peff.png"),
       p_irrDepth_Peff, width = 190, height = 195, units = "mm")
