## Map+Dens-ETbyField-Annual.R

source(file.path("code", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

# figure out extent
bound <- sf::st_buffer(st_transform(sf_lema, st_crs(sf_fields)), dist = units::set_units(8000, "m"))
extent <- sf::st_bbox(st_transform(sf_lema, st_crs(sf_fields)))
sf_subset <- sf_fields[bound, op = st_within]

# et data
df_et_yr <- 
  readr::read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "OpenET_EstimateFieldIrrigation-Annual_FieldsNoDups.csv")) |>  # 2016-2021
  subset(Year <= 2020) |> 
  left_join(fields_spatial, by = "UID") |> 
  mutate(ET.P_mm = ET_mm - precip_mm) |> 
  select(-irr_m3_fromPrec, -irr_mm_fromNonIrr, -irr_m3_fromNonIrr)

# plot ET by field
sf_et_yr <- right_join(sf_subset, df_et_yr, by = "UID")

## MAPS
p_et_map <-
  ggplot(sf_et_yr) +
  geom_sf(aes(fill = ET_mm), color = NA) +
  geom_sf(data = sf_lema, color = "red", fill = NA) +
  facet_grid(Algorithm ~ Year) +
  coord_sf(xlim = c(extent["xmin"]-1000, extent["xmax"]+1000), 
           ylim = c(extent["ymin"]-1000, extent["ymax"]+1000)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(name = "Annual ET [mm]", direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.75)) +
  NULL

ggsave(file.path("figures+tables", "Map-ETbyField-AnnualET.png"),
       p_et_map, width = 190, height = 210, units = "mm")

p_et.p_map <-
  ggplot(sf_et_yr) +
  geom_sf(aes(fill = ET.P_mm), color = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA, size = 1) +
  facet_grid(Algorithm ~ Year) +
  coord_sf(xlim = c(extent["xmin"]-1000, extent["xmax"]+1000), 
           ylim = c(extent["ymin"]-1000, extent["ymax"]+1000)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradient2(name = "Annual ET - P [mm]",
                       low = col.cat.blu, high = col.cat.red) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.75)) +
  NULL

ggsave(file.path("figures+tables", "Map-ETbyField-AnnualET-P.png"),
       p_et.p_map, width = 190, height = 210, units = "mm")

p_irr_map <-
  ggplot(sf_et_yr) +
  geom_sf(aes(fill = irr_mm_fromPrec), color = NA) +
  geom_sf(data = sf_lema, color = "red", fill = NA, size = 1) +
  facet_grid(Algorithm ~ Year) +
  coord_sf(xlim = c(extent["xmin"]-1000, extent["xmax"]+1000), 
           ylim = c(extent["ymin"]-1000, extent["ymax"]+1000)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(name = "Estimated Irrigation [mm]", direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.75)) +
  NULL

ggsave(file.path("figures+tables", "Map-ETbyField-AnnualIrr.png"),
       p_irr_map, width = 190, height = 210, units = "mm")

## DENSITY PLOTS
# only irrigated fields in LEMA
p_et_dens <-
  df_et_yr |> 
  subset(IrrigatedPrc > 0.5 & IrrConfidence == "High" & within_lema) |> 
  ggplot() +
  geom_density(aes(x = ET_mm, fill = Algorithm, color = Algorithm), alpha = 0.2) +
  facet_wrap( ~ Year) +
  scale_x_continuous(name = "Annual ET [mm]") +
  scale_color_brewer(labels = labs_algorithms, type = "qual") +
  scale_fill_brewer(labels = labs_algorithms, type = "qual") +
  theme(legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "Dens_ETbyField-AnnualET.png"), p_et_dens,
       width = 150, height = 120, units = "mm")

p_et.p_dens <-
  df_et_yr |> 
  subset(IrrigatedPrc > 0.5 & IrrConfidence == "High" & within_lema) |> 
  ggplot() +
  annotate("rect", xmin = 200, xmax = 300, ymin = -Inf, ymax = Inf, fill = col.gray, color = NA, alpha = 0.5) +
  geom_density(aes(x = ET.P_mm, fill = Algorithm, color = Algorithm), alpha = 0.2) +
  facet_wrap( ~ Year) +
  scale_x_continuous(name = "Annual ET - P [mm]") +
  scale_color_brewer(labels = labs_algorithms, type = "qual") +
  scale_fill_brewer(labels = labs_algorithms, type = "qual") +
  theme(legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "Dens_ETbyField-AnnualET-P.png"), p_et.p_dens,
       width = 150, height = 120, units = "mm")
