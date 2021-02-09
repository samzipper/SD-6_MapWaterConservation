## DataPrep_03_WRgroups_SummarizeAnnualData.R
# This script is intended to summarize annual ET, land cover, and irrigation status for each WR group.

source(file.path("code", "paths+packages.R"))

## load WR groups
# output from script DataPrep_02_WRgroups_SummarizeUse+Fields.R
wrg_fields <- readr::read_csv(file.path("data", "WRgroups_FieldByYear.csv"))
wrg_summary <- readr::read_csv(file.path("data", "WRgroups_UseByWRG.csv"))

## first: summarize land cover and irrigation status
# output from script DataPrep_01_Fields_SeparateBoundaries+Attributes.R
irr_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) %>% 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
lc_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) %>% 
  dplyr::select(-starts_with("pctcov")) %>% 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode")
att_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

## summarize ET data
# ET rates
yr_range <- seq(2016,2018) # only years that have LULC data
alg_all <- c("ensemble", "disalexi", "eemetric", "ptjpl", "sims", "ssebop")
for (alg in alg_all){
  for (yr in yr_range){
    et_fields_y <- 
      file.path(dir_data, "OpenET", paste0("testing_multi_mean_ythsn82poy_", alg, "_et_", yr, ".csv")) %>% 
      readr::read_csv() %>% 
      dplyr::select(-.geo, -`system:index`) %>% 
      # because of duplicated polygons (see bottom of DataPrep_Fields_01_SeparateBoundaries+Attributes.R for more info)
      # there are a few duplicated UIDs. take the mean of these to get a single value for UID because all other data
      # are aggregated to the UID scale.
      dplyr::group_by(UID, date) %>% 
      dplyr::summarize(et_field_mean = mean(mean)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(Algorithm = alg)
    
    if (alg == alg_all[1] & yr == yr_range[1]){
      et_fields_mo <- et_fields_y
    } else {
      et_fields_mo <- dplyr::bind_rows(et_fields_mo, et_fields_y)
    }
  }
}

et_fields_yr <- 
  et_fields_mo %>% 
  dplyr::mutate(Year = lubridate::year(date)) %>% 
  dplyr::group_by(UID, Year, Algorithm) %>% 
  dplyr::summarize(ET_mm = round(sum(et_field_mean), 2)) %>% 
  dplyr::ungroup()

## combine ET with wrg, field spatial attributes, land cover
## combine field spatial attributes and land cover with WR groups
alldata_fields <- 
  et_fields_yr %>% 
  dplyr::left_join(att_fields, by = "UID") %>% 
  dplyr::left_join(wrg_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(irr_fields, by = c("UID", "Year"))

## aggregate to WR group
# first, identify dominant crop type by WR group
crops_wrg <- 
  alldata_fields %>% 
  dplyr::select(UID, Year, area_m2, WR_GROUP, CropCode) %>% 
  unique() %>% 
  dplyr::group_by(Year, WR_GROUP, CropCode) %>% 
  dplyr::summarize(area_m2_crop = sum(area_m2)) %>% 
  dplyr::group_by(Year, WR_GROUP) %>% 
  dplyr::filter(area_m2_crop == max(area_m2_crop)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(area_m2_maincrop = area_m2_crop,
                CropCode_maincrop = CropCode)

# now, summarize other characteristics
alldata_wrg <- 
  alldata_fields %>% 
  subset(is.finite(WR_GROUP)) %>%  # non-irrigated fields have NA for WR_GROUP
  dplyr::group_by(WR_GROUP, Year, Algorithm) %>% 
  dplyr::summarize(n_irrfields = length(unique(UID)), # all fields are irrigated, so this will be equal to # of irrigated fields
                   area_m2_wrg = round(sum(area_m2), 2),
                   n_within_lema = sum(within_lema),
                   n_within_buffer = sum(within_buffer),
                   n_croptypes = length(unique(CropCode)),
                   ET_m3 = round(sum((ET_mm/1000)*area_m2), 2)) %>% 
  dplyr::left_join(crops_wrg, by = c("Year", "WR_GROUP")) %>% 
  dplyr::mutate(area_prc_maincrop = area_m2_maincrop/area_m2_wrg) %>% 
  dplyr::left_join(wrg_summary, by = c("Year", "WR_GROUP"))

# round some files
alldata_wrg$Irrigation_m3 <- round(alldata_wrg$Irrigation_m3, 2)
alldata_wrg$IrrArea_m2 <- round(alldata_wrg$IrrArea_m2, 2)
alldata_wrg$WaterUse_m3 <- round(alldata_wrg$WaterUse_m3, 2)

## save output
# data by water rights group
alldata_wrg %>% 
  dplyr::select(WR_GROUP, Year, Algorithm, ET_m3, Irrigation_m3, IrrArea_m2, WaterUse_m3, area_m2_wrg, 
                n_irrfields, n_within_lema, n_within_buffer, CropCode_maincrop, area_m2_maincrop, n_croptypes) %>% 
  readr::write_csv(file.path("data", "WRgroups_AnnualData.csv"))

# ET data for all fields
alldata_fields %>% 
  dplyr::select(UID, Year, Algorithm, ET_mm) %>% 
  readr::write_csv(file.path("data", "Fields_AnnualET.csv"))

## QA/QC
# compare wrg irrigated area and fields irrigated area
ggplot(subset(alldata_wrg, Algorithm == "ensemble"), aes(x = IrrArea_m2/10000, y = area_m2_wrg/10000)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Irrigated area [ha], reported") +
  scale_y_continuous(name = "Irrigated area [ha], inferred") +
  labs(title = "Irrigated area within each water rights group") +
  ggsave(file.path("plots", "WRgroups_CompareIrrigatedAcres.png"),
         width = 95, height = 95, units = "mm")

# compare wrg irrigation volume to ET volume
ggplot(alldata_wrg, aes(x = Irrigation_m3, y = ET_m3)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_grid(Algorithm ~ Year) +
  scale_x_continuous(name = "Reported Irrigation [m\u00b3]") +
  scale_y_continuous(name = "Estimated ET [m\u00b3]") +
  labs(title = "Comparison of irrigation vs. ET volumes by water rights group") +
  ggsave(file.path("plots", "WRgroups_CompareETtoIrrigation.png"),
         width = 190, height = 240, units = "mm")
