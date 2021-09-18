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

# ET rates
yr_range <- seq(2016,2020)
alg <- "ensemble"
for (yr in yr_range){
  et_fields_y <- 
    file.path(dir_data, "OpenET", paste0("testing_multi_mean_ythsn82poy_", alg, "_et_", yr, ".csv")) %>% 
    readr::read_csv() %>% 
    dplyr::select(-.geo, -`system:index`) %>% 
    dplyr::mutate(Algorithm = alg)
  
  if (yr == yr_range[1]){
    et_fields_mo <- et_fields_y
  } else {
    et_fields_mo <- dplyr::bind_rows(et_fields_mo, et_fields_y)
  }
}

et_fields_yr <- 
  et_fields_mo %>% 
  dplyr::mutate(Year = lubridate::year(date)) %>% 
  dplyr::group_by(UID, Year, Algorithm) %>% 
  dplyr::summarize(ET_mm = sum(mean)) %>% 
  dplyr::ungroup()

# combine everything to plot
df_data <- 
  dplyr::left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) %>% 
  dplyr::left_join(fields_spatial, by = "UID") %>% 
  dplyr::left_join(et_fields_yr, by = c("Year", "UID")) %>% 
  subset(Year %in% c(2016, 2017) &
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
