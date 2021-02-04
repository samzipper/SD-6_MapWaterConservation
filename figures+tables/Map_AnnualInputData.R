## Map_AnnualInputData.R
# Map annual ET, crop type, irrigation status.

source(file.path("code", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields <- sf::st_read(file.path("data", "Fields_Boundaries.gpkg"))
fields_irrigation <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) %>% 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
fields_landcover <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) %>% 
  dplyr::select(-starts_with("pctcov")) %>% 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode")

# ET rates
yr_range <- seq(2016,2020)
alg <- "ensemble"
for (yr in yr_range){
  df_et_y <- 
    file.path(dir_data, "OpenET", paste0("testing_multi_mean_ythsn82poy_", alg, "_et_", yr, ".csv")) %>% 
    readr::read_csv() %>% 
    dplyr::select(-.geo, -`system:index`) %>% 
    dplyr::mutate(Algorithm = alg)
  
  if (yr == yr_range[1]){
    df_et_mo <- df_et_y
  } else {
    df_et_mo <- dplyr::bind_rows(df_et_mo, df_et_y)
  }
}

df_et_yr <- 
  df_et_mo %>% 
  dplyr::mutate(Year = lubridate::year(date)) %>% 
  dplyr::group_by(UID, Year, Algorithm) %>% 
  dplyr::summarize(ET_mm = sum(mean)) %>% 
  dplyr::ungroup()

# combine with ET and crop type
sf_all <-
  dplyr::full_join(sf_fields, fields_irrigation, by = c("UID")) %>% 
  dplyr::left_join(fields_landcover, by = c("UID", "Year")) %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode") %>% 
  dplyr::left_join(df_et_yr, by = c("UID", "Year"))

# figure out extent
bound <- sf::st_buffer(st_transform(sf_buff, st_crs(sf_all)), dist = units::set_units(8000, "m"))
extent <- sf::st_bbox(st_transform(sf_buff, st_crs(sf_all)))
sf_subset <- sf_all[bound, op = st_intersects]

# plot
p_irr <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% c(2016, 2017)), aes(fill = factor(Irrigation)), color = NA) +
  geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = "Irrigation Status",
                    values = c("0" = col.gray, "1" = col.cat.blu),
                    labels = c("0" = "Non-Irrigated", "1" = "Irrigated")) +
  theme(legend.position = "bottom") +
  NULL

p_lc <-
  ggplot() +
  geom_sf(data = subset(sf_all, Year %in% c(2016, 2017)), aes(fill = CropGroup), color = NA) +
  geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = "Land Cover", values = pal_crops) +
  theme(legend.position = "bottom") +
  NULL

p_et <-
  ggplot() +
  geom_sf(data = subset(sf_all, Year %in% c(2016, 2017)), aes(fill = ET_mm), color = NA) +
  geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = "ET [mm]", limits = quantile(df_et_yr$ET_mm, c(0.005, 0.995)),
                       guide = guide_colorbar(barwidth = 20)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL

(p_et + p_irr + p_lc) +
  plot_layout(ncol = 1) +
  ggsave(file.path("figures+tables", "Map_AnnualInputData.png"),
         width = 190, height = 240, units = "mm")
