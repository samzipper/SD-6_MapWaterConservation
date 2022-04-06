## Map_AnnualInputData.R
# Map annual ET, crop type, irrigation status.

source(file.path("code", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))
irr_fields <- readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) # 1984-2020
lc_fields  <- readr::read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv"))  # 2006-2020

# annual ET
df_et_yr <- readr::read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Annual_All_FieldsNoDups.csv")) # 2016-2021

# years to subset/plot
yrs_common <- seq(2016, 2020)

# combine with ET and crop type
sf_all <-
  dplyr::full_join(sf_fields, subset(irr_fields, Year %in% yrs_common), by = c("UID")) %>% 
  dplyr::left_join(lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode") %>% 
  dplyr::left_join(subset(df_et_yr, Algorithm == "ensemble"), by = c("UID", "Year"))

# figure out extent
bound <- sf::st_buffer(st_transform(sf_lema, st_crs(sf_all)), dist = units::set_units(7000, "m"))
extent <- sf::st_bbox(st_transform(sf_lema, st_crs(sf_all)))
sf_subset <- sf_all[bound, op = st_intersects]

# plot
yrs_plot <- seq(2016, 2020)

quantile(subset(sf_subset, Year %in% yrs_plot & Algorithm == "ensemble")$ET_mm, c(0,1))
quantile(subset(sf_subset, Year %in% yrs_plot & Algorithm == "ensemble")$ET_mm, c(0.01,0.99))

p_et <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% yrs_plot & Algorithm == "ensemble"), aes(fill = ET_mm), color = NA) +
  #geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year, nrow = 1) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  #scale_fill_viridis_c(name = "ET [mm]", limits = quantile(df_et_yr$ET_mm, c(0.005, 0.995), na.rm = T),
  #                     guide = guide_colorbar(barwidth = 20)) +
  scale_fill_viridis_c(name = "Annual ET [mm]", direction = -1, limits = c(500, 1000), breaks = c(500, 750, 1000)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL

p_irr <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% yrs_plot), aes(fill = IrrigatedPrc), color = NA) +
  #geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year, nrow = 1) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = "Irrigation %", breaks = c(0, 0.5, 1), labels = scales::percent, option = "E", direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL

table(subset(sf_subset, Algorithm == "ensemble")$CropGroupCoarse)

p_lc <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% yrs_plot), aes(fill = CropGroupCoarse), color = NA) +
  #geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year, nrow = 1) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = "Land Cover", values = pal_crops_coarse, drop = T) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL

p_combo <- 
  (p_et + p_irr + p_lc) +
  plot_layout(ncol = 1, heights = c(1, 1, 1))

ggsave(file.path("figures+tables", "Map_AnnualInputData.png"),
       p_combo, width = 190, height = 150, units = "mm")
