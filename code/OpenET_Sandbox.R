## OpenET_Sandbox.R
# Script for exploratory analysis of OpenET data.

source(file.path("code", "paths+packages.R"))

# field attributes
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) %>% 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
fields_landcover <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) %>% 
  dplyr::select(-starts_with("pctcov")) %>% 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode") %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode")
met_yearly_fields <- readr::read_csv(file.path("data", "gridmet_AnnualByField.csv"))

# shapefiles
fields_sf <- st_read(file.path("data", "Fields_NoDups.shp"))
lema_sf <- st_read(file.path("data", "SD6_outline.gpkg"))
buffer_sf <- st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# ET rates
et_fields_mo <- 
  file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Monthly_All_FieldsNoDups.csv") %>% 
  read_csv() %>% 
  pivot_longer(starts_with("ET_mm_"), values_to = "ET_mm") %>% 
  mutate(Algorithm = str_sub(name, start = 12)) %>% 
  dplyr::select(-name) %>% 
  subset(year(Date) <= 2020)
  
et_fields_yr <- 
  file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Annual_All_FieldsNoDups.csv") %>% 
  read_csv() %>% 
  subset(Year <= 2020)

# combine data for all fields/years
fields_sf_with_et <-
  right_join(fields_sf, et_fields_yr, by = "UID") %>% 
  left_join(met_yearly_fields, by = c("UID", "Year")) %>% 
  mutate(PrecipDefc_mm = ET_mm - precip_mm) %>% 
  left_join(fields_spatial, by = "UID")

# figure out extent
bound <- sf::st_buffer(st_transform(lema_sf, st_crs(fields_sf_with_et)), dist = units::set_units(7000, "m"))
extent <- sf::st_bbox(st_transform(lema_sf, st_crs(fields_sf_with_et)))

# map annual ET
p_annualET <- 
  ggplot(fields_sf_with_et) +
  geom_sf(aes(fill = ET_mm, geometry = geometry), color = NA) +
  #geom_sf(data = buffer_sf, color = "red", fill = NA) +
  geom_sf(data = lema_sf, color = "blue", fill = NA) +
  facet_grid(Algorithm~Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = "[mm]") +
  labs(title = "Annual ET by year and algorithm") +
  theme(axis.text = element_blank())
ggsave(file.path("plots", "OpenET_MapAnnualETbyAlgorithm.png"), p_annualET,
       width = 250, height = 180, units = "mm")

# map annual precipitation deficit
p_annualDefc <- 
  ggplot(fields_sf_with_et) +
  geom_sf(aes(fill = PrecipDefc_mm, geometry = geometry), color = NA) +
  #geom_sf(data = buffer_sf, color = "red", fill = NA) +
  geom_sf(data = lema_sf, color = "blue", fill = NA) +
  facet_grid(Algorithm~Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_gradient2(name = "[mm]") +
  labs(title = "Annual (ET - precipitation) by year and algorithm") +
  theme(axis.text = element_blank())
ggsave(file.path("plots", "OpenET_MapAnnualDefcByAlgorithm.png"), p_annualDefc,
       width = 250, height = 180, units = "mm")

# density plot annual ET
p_annualETdens <- 
  ggplot(subset(fields_sf_with_et, within_lema)) +
  geom_density(aes(x = ET_mm, color = Algorithm, fill = Algorithm), alpha = 0.2) +
  facet_wrap(~Year) +
  scale_color_brewer(name = "Algorithm", type = "qual") +
  scale_fill_brewer(name = "Algorithm", type = "qual") +
  labs(title = "Annual ET by year and algorithm", subtitle = "LEMA only")
ggsave(file.path("plots", "OpenET_DensAnnualETByAlgorithm.png"), p_annualETdens,
       width = 150, height = 120, units = "mm")

# density plot annual precipitation deficit
p_annualETdens <- 
  ggplot(subset(fields_sf_with_et, within_lema)) +
  geom_vline(xintercept = 0, color = col.gray) +
  annotate("rect", xmin = 200, xmax = 300, ymin = -Inf, ymax = Inf, fill = col.gray, color = NA, alpha = 0.5) +
  geom_density(aes(x = PrecipDefc_mm, color = Algorithm, fill = Algorithm), alpha = 0.2) +
  facet_wrap(~Year) +
  scale_color_brewer(name = "Algorithm", type = "qual") +
  scale_fill_brewer(name = "Algorithm", type = "qual") +
  labs(title = "Annual (ET - precipitation) by year and algorithm", subtitle = "LEMA only")
ggsave(file.path("plots", "OpenET_DensAnnualDefcByAlgorithm.png"), p_annualETdens,
       width = 150, height = 120, units = "mm")




# combine everything to plot
df_data <- 
  dplyr::left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) %>% 
  dplyr::left_join(fields_spatial, by = "UID") %>% 
  dplyr::left_join(et_fields_yr, by = c("Year", "UID")) %>% 
  subset(Year %in% c(2016, 2017, 2018) &
           (within_lema | within_buffer))

# compare irrigated/nonirrigated corn, inside and outside buffer
ggplot(subset(df_data, CropGroup == "Corn"), 
       aes(x = factor(Year), y = ET_mm, color = factor(Irrigation), fill = within_lema)) +
  geom_boxplot(position = "dodge") +
  scale_fill_manual(name = "Location", 
                    labels = c("FALSE" = "Buffer", "TRUE" = "LEMA"),
                    values = c("FALSE" = col.gray, "TRUE" = col.cat.yel)) +
  scale_color_manual(name = "Irrigation Status", 
                     labels = c("0" = "Non-Irrigated", "1" = "Irrigated"),
                     values = c("0" = col.cat.org, "1" = col.cat.blu)) +
  ggsave(file.path("plots", "OpenET_Sandbox_CornBoxplots.png"),
         width = 140, height = 95, units = "mm")

## plot monthly ET from corn inside/outside lema
df_mo <-
  et_fields_mo %>% 
  mutate(Year = year(date)) %>% 
  left_join(fields_irrigation, by = c("Year", "UID")) %>% 
  left_join(fields_landcover, by = c("Year", "UID")) %>% 
  left_join(fields_spatial, by = c("UID")) %>% 
  subset(Year %in% c(2016, 2017) &
           (within_lema | within_buffer))

df_mo_crop <- 
  subset(df_mo, Year == 2016 & CropCode %in% c(1, 4, 5)) %>% 
  group_by(date, Algorithm, CropName, Irrigation) %>% 
  summarize(ET_min = min(mean),
            ET_prc25 = quantile(mean, 0.25),
            ET_prc50 = quantile(mean, 0.5),
            ET_prc75 = quantile(mean, 0.75),
            ET_max = max(mean))

# plot only corn
df_mo_corn <- 
  subset(df_mo, Year == 2016 & CropCode == 1 & Irrigation == 1)

df_mo_corn_median <- 
  df_mo_corn %>% 
  group_by(date) %>% 
  summarize(ET_mean = mean(mean),
            ET_median = median(mean))


p_corn <-
  ggplot() +
  geom_line(data = df_mo_corn, aes(x = date, y = mean, group = UID), alpha = 0.25, color = col.gray) +
  geom_line(data = df_mo_corn_median, aes(x = date, y = ET_mean), color = pal_crops[1]) +
  scale_x_date(name = "Month", expand = c(0,0),
               date_breaks = "1 month",
               labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J")) +
  scale_y_continuous(name = "ET [mm],OpenET\nEnsemble Mean",
                     limits = c(0, max(df_mo_corn$mean)), 
                     expand = c(0,0))

p_comparecrops <-
  ggplot(df_mo_crop, aes(x = date)) +
  geom_line(aes(y = ET_prc50, color = CropName, linetype = factor(Irrigation))) +
  scale_color_manual(name = "Crop", values = pal_crops[1:3]) +
  scale_x_date(name = "Month", expand = c(0,0),
               date_breaks = "1 month",
               labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J")) +
  scale_y_continuous(name = "ET [mm],OpenET\nEnsemble Mean",
                     limits = c(0, max(df_mo_corn$mean)), 
                     expand = c(0,0)) +
  scale_linetype_manual(name = "Irrigation Status", labels = c("0" = "Rainfed", "1" = "Irrigated"),
                        values = c("0" = "dashed", "1" = "solid")) +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(direction = "vertical", order = 2),
         color = guide_legend(direction = "vertical", order = 1))

p_combo <-
  (p_corn + p_comparecrops) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")
ggsave(file.path("plots", "OpenET_CompareCropTimeseries.png"),
       p_combo, 
       width = 95, height = 125, units = "mm")
