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
