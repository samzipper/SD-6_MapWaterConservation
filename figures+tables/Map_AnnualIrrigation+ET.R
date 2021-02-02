## Map_AnnualIrrigation+ET.R
# Map annual ET from OpenET data

source(file.path("src", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries
sf_fields <- sf::st_read(file.path(dir_GIS, "landUse+irrigation", "CLU_SPLIT_ZM_inside_SD6buf10mi_CDL_2008_2018.shp"))

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
  dplyr::summarize(ET_mm = sum(mean))

# reorganize crop type and irrigation status to long form
yrs_fields <- seq(2006, 2018)
for (yrf in yrs_fields){
  sf_fields_yr <-
    sf_fields %>% 
    dplyr::select(UID, ends_with(as.character(yrf))) %>% 
    dplyr::rename_with(~(gsub(yrf, "", .x, fixed = TRUE))) %>% 
    dplyr::mutate(Year = yrf)
  
  if (yrf == yrs_fields[1]){
    sf_fields_long <- sf_fields_yr
  } else {
    sf_fields_long <- dplyr::bind_rows(sf_fields_long, sf_fields_yr)
  }
  
}

# combine with ET and crop type
sf_all <-
  dplyr::left_join(sf_fields_long, df_et_yr, by = c("UID", "Year")) %>% 
  dplyr::ungroup()

# figure out extent
extent <- sf::st_bbox(st_transform(sf_buff, st_crs(sf_all)))

# plot
p_irr <-
  ggplot() +
  geom_sf(data = subset(sf_all, Year %in% c(2016, 2017)), aes(geometry = geometry, fill = factor(irr))) +
  geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = "Irrigation Status",
                    values = c("0" = "gray65", "1" = "forestgreen"),
                    labels = c("0" = "Non-Irrigated", "1" = "Irrigated")) +
  theme(legend.position = "bottom") +
  NULL

p_et <-
  ggplot() +
  geom_sf(data = subset(sf_all, Year %in% c(2016, 2017)), aes(geometry = geometry, fill = ET_mm)) +
  geom_sf(data = sf_buff, color = "red", fill = NA, size = 1) +
  geom_sf(data = sf_lema, color = "blue", fill = NA, size = 1) +
  facet_wrap(~ Year) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = "ET [mm]", limits = quantile(df_et_yr$ET_mm, c(0.005, 0.995))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  NULL

(p_irr + p_et) +
  plot_layout(ncol = 1) +
  ggsave(file.path("figures+tables", "Map_AnnualIrrigation+ET.png"),
         width = 190, height = 190, units = "mm")
