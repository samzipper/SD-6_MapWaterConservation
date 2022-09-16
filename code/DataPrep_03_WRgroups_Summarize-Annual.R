## DataPrep_03_WRgroups_Summarize-Annual.R
# This script is intended to summarize annual ET, land cover, and irrigation status for each WR group.

source(file.path("code", "paths+packages.R"))

## load WR groups
# output from script DataPrep_02_WRgroups_SummarizeUse+Fields.R
wrg_fields <- readr::read_csv(file.path("data", "WRgroups_FieldByYear.csv"))
wrg_summary <- readr::read_csv(file.path("data", "WRgroups_UseByWRG.csv"))

## first: summarize land cover and irrigation status
# output from script DataPrep_01_Fields_SeparateBoundaries+Attributes.R
fields_irrigation <- read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "OpenET_EstimateFieldIrrigation-Annual_FieldsNoDups.csv"))

fields_landcover <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

att_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

## combine ET with wrg, field spatial attributes, land cover
alldata_fields <- 
  fields_irrigation |> 
  dplyr::left_join(att_fields, by = "UID") |> 
  dplyr::left_join(wrg_fields, by = c("UID", "Year")) |> 
  dplyr::left_join(fields_landcover, by = c("UID", "Year"))

## aggregate to WR group
# first, identify dominant crop type by WR group
crops_wrg <- 
  alldata_fields |> 
  dplyr::select(UID, Year, area_m2, WR_GROUP, CropCode) |> 
  unique() |> 
  dplyr::group_by(Year, WR_GROUP, CropCode) |> 
  dplyr::summarize(area_m2_crop = sum(area_m2)) |> 
  dplyr::group_by(Year, WR_GROUP) |> 
  dplyr::filter(area_m2_crop == max(area_m2_crop)) |> 
  dplyr::ungroup() |> 
  dplyr::rename(area_m2_maincrop = area_m2_crop,
                CropCode_maincrop = CropCode)

# now, summarize other characteristics
alldata_wrg <- 
  alldata_fields |> 
  subset(is.finite(WR_GROUP)) |>  # non-irrigated fields have NA for WR_GROUP
  dplyr::group_by(WR_GROUP, Year, Algorithm) |> 
  dplyr::summarize(n_irrfields = length(unique(UID)), # all fields are irrigated, so this will be equal to # of irrigated fields
                   n_IrrConfHigh = sum(IrrConfidence == "High"),
                   area_m2_wrg = round(sum(area_m2), 2),
                   n_within_lema = sum(within_lema),
                   n_within_buffer = sum(within_buffer),
                   n_croptypes = length(unique(CropCode)),
                   ET_m3 = round(sum((ET_mm/1000)*area_m2), 2),
                   irr_m3_fromNonIrr = round(sum(irr_m3_fromNonIrr), 2),
                   irr_m3_fromPrec = round(sum(irr_m3_fromPrec), 2)) |> 
  dplyr::left_join(crops_wrg, by = c("Year", "WR_GROUP")) |> 
  dplyr::mutate(area_prc_maincrop = area_m2_maincrop/area_m2_wrg) |> 
  dplyr::left_join(wrg_summary, by = c("Year", "WR_GROUP"))

# round some files
alldata_wrg$IrrArea_m2 <- round(alldata_wrg$IrrArea_m2, 2)

# drop WaterUse_m3, which included non-irrigation water
alldata_wrg$WaterUse_m3 <- NULL

# rename Irrigation_m3 to irr_m3_fromWIMAS
names(alldata_wrg)[names(alldata_wrg) == "Irrigation_m3"] <- "irr_m3_fromWIMAS"

## save output
# data by water rights group
readr::write_csv(alldata_wrg, file.path("data", "WRgroups_Summarize-Annual.csv"))

## QA/QC
# more high confidence fields than irrigated fields?
which(alldata_wrg$n_IrrConfHigh > alldata_wrg$n_irrfields) # WRGROUP 458, year 2016 - 2 fields, UID 348228 and 1430796

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
ggplot(alldata_wrg, aes(x = Irrigation_m3, y = irr_m3_fromNonIrr)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_grid(Algorithm ~ Year) +
  scale_x_continuous(name = "Reported Irrigation [m\u00b3]") +
  scale_y_continuous(name = "Estimated ET Surplus (actual ET - nonirrigated ET for those crops) [m\u00b3]") +
  labs(title = "Comparison of irrigation vs. ET surplus volumes by water rights group")
ggsave(file.path("plots", "WRgroups_CompareETsurplustoIrrigation.png"),
       width = 190, height = 240, units = "mm")
