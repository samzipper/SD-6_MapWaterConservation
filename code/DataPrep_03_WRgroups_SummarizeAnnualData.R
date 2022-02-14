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

# ET rates
et_fields_mo <- 
  file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Monthly_All_FieldsNoDups.csv") %>% 
  read_csv() %>% 
  pivot_longer(starts_with("ET_mm_"), values_to = "ET_mm") %>% 
  mutate(Algorithm = str_sub(name, start = 12)) %>% 
  dplyr::select(-name) %>% 
  subset(year(Date) %in% yr_range)

et_fields_yr <- 
  file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Annual_All_FieldsNoDups.csv") %>% 
  read_csv() %>% 
  subset(Year %in% yr_range)

## summarize meteorological data
met_keys <- 
  file.path(dir_data, "gridMET", "gridMET_Keys.csv") %>% 
  readr::read_csv(col_types = "cdc") %>% 
  dplyr::select(-fid) %>% 
  dplyr::rename(gridmet_id = masterid)
readr::write_csv(met_keys, file.path("data", "gridmet_GridmetToFieldsKey.csv"))

met_eto <- 
  file.path(dir_data, "gridMET", "monthly_eto.csv") %>% 
  readr::read_csv() %>% 
  tidyr::pivot_longer(cols = -`...1`, names_to = "masterid", values_to = "ETo_mm") %>% 
  dplyr::rename(datetime = `...1`)
met_etr <- 
  file.path(dir_data, "gridMET", "monthly_etr.csv") %>% 
  readr::read_csv() %>% 
  tidyr::pivot_longer(cols = -`...1`, names_to = "masterid", values_to = "ETr_mm") %>% 
  dplyr::rename(datetime = `...1`)
met_precip <- 
  file.path(dir_data, "gridMET", "monthly_precip.csv") %>% 
  readr::read_csv() %>% 
  tidyr::pivot_longer(cols = -`...1`, names_to = "masterid", values_to = "precip_mm") %>% 
  dplyr::rename(datetime = `...1`)

met_monthly <- 
  dplyr::left_join(met_eto, met_etr, by = c("datetime", "masterid")) %>% 
  dplyr::left_join(met_precip, by = c("datetime", "masterid")) %>% 
  dplyr::rename(gridmet_id = masterid)
readr::write_csv(met_monthly, file.path("data", "gridmet_MonthlyByGridmet.csv"))

met_yearly <-
  met_monthly %>% 
  dplyr::mutate(Year = lubridate::year(datetime)) %>% 
  dplyr::group_by(gridmet_id, Year) %>% 
  dplyr::summarize(ETo_mm = round(sum(ETo_mm), 2),
                   ETr_mm = round(sum(ETr_mm), 2),
                   precip_mm = round(sum(precip_mm), 2)) %>% 
  subset(Year >= 2006) # this is when land cover data starts
readr::write_csv(met_yearly, file.path("data", "gridmet_AnnualByGridmet.csv"))

met_yearly_fields <-
  dplyr::left_join(met_keys, met_yearly, by = "gridmet_id") %>% 
  dplyr::select(-gridmet_id)
readr::write_csv(met_yearly_fields, file.path("data", "gridmet_AnnualByField.csv"))

## combine ET with wrg, field spatial attributes, land cover
## combine field spatial attributes and land cover with WR groups
alldata_fields <- 
  et_fields_yr %>% 
  dplyr::left_join(att_fields, by = "UID") %>% 
  dplyr::left_join(wrg_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(irr_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(met_yearly_fields, by = c("UID", "Year"))

## summarize non-irrigated ET by crop type
# set some criteria for which fields to keep for estimating non-irrigated ET
UIDs_keep <- att_fields$UID[att_fields$within_buffer | att_fields$within_lema]  # within LEMA or buffer
area_m2_thres <- 60703 # 60703 m2 = 15 acres

et_nonirr <- 
  alldata_fields %>% 
  subset(UID %in% UIDs_keep & Irrigation == 0 & area_m2 > area_m2_thres) %>% 
  dplyr::group_by(Year, Algorithm, CropCode) %>% 
  dplyr::summarize(ET_mm_CropNonirrMedian = median(ET_mm),
                   ETo_mm_CropNonirrMedian = median(ETo_mm),
                   ETr_mm_CropNonirrMedian = median(ETr_mm))

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
  dplyr::left_join(et_nonirr, by = c("CropCode", "Year", "Algorithm")) %>% 
  subset(is.finite(WR_GROUP)) %>%  # non-irrigated fields have NA for WR_GROUP
  dplyr::group_by(WR_GROUP, Year, Algorithm) %>% 
  dplyr::summarize(n_irrfields = length(unique(UID)), # all fields are irrigated, so this will be equal to # of irrigated fields
                   area_m2_wrg = round(sum(area_m2), 2),
                   n_within_lema = sum(within_lema),
                   n_within_buffer = sum(within_buffer),
                   n_croptypes = length(unique(CropCode)),
                   ET_m3 = round(sum((ET_mm/1000)*area_m2), 2),
                   ET_m3_nonirr = round(sum((ET_mm_CropNonirrMedian/1000)*area_m2), 2),
                   ETo_m3 = round(sum((ETo_mm/1000)*area_m2), 2),
                   ETr_m3 = round(sum((ETr_mm/1000)*area_m2), 2),
                   precip_m3 = round(sum((precip_mm/1000)*area_m2), 2)) %>% 
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
  dplyr::select(WR_GROUP, Year, Algorithm, ET_m3, ETo_m3, ETr_m3, precip_m3, ET_m3_nonirr, 
                Irrigation_m3, IrrArea_m2, WaterUse_m3, area_m2_wrg, 
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
  labs(title = "Irrigated area within each water rights group")
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
  labs(title = "Comparison of irrigation vs. ET volumes by water rights group")
ggsave(file.path("plots", "WRgroups_CompareETtoIrrigation.png"),
       width = 190, height = 240, units = "mm")

# compare wrg irrigation volume to (ET - ET_nonirr) volume
alldata_wrg$ET_m3_surplus <- alldata_wrg$ET_m3 - alldata_wrg$ET_m3_nonirr
lm(ET_m3_surplus ~ Irrigation_m3, data = alldata_wrg)
ggplot(alldata_wrg, aes(x = Irrigation_m3, y = ET_m3_surplus)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_grid(Algorithm ~ Year) +
  scale_x_continuous(name = "Reported Irrigation [m\u00b3]") +
  scale_y_continuous(name = "Estimated ET Surplus (actual ET - nonirrigated ET for those crops) [m\u00b3]") +
  labs(title = "Comparison of irrigation vs. ET surplus volumes by water rights group")
ggsave(file.path("plots", "WRgroups_CompareETsurplustoIrrigation.png"),
       width = 190, height = 240, units = "mm")
