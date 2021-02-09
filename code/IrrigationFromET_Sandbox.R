## IrrigationFromET_Sandbox.R
# Estimate irrigation from ET.

source(file.path("code", "paths+packages.R"))

## load data
# annual ET and wr group summaries (from DataPrep_03_WRgroups_SummarizeAnnualData.R)
alldata_wrg <- readr::read_csv(file.path("data", "WRgroups_AnnualData.csv"))
et_fields_yr <- readr::read_csv(file.path("data", "Fields_AnnualET.csv"))

# land cover and irrigation status (from DataPrep_01_Fields_SeparateBoundaries+Attributes.R)
irr_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) %>% 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
lc_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) %>% 
  dplyr::select(-starts_with("pctcov")) %>% 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode")
att_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

## combine all data
alldata_fields <- 
  et_fields_yr %>% 
  dplyr::left_join(att_fields, by = "UID") %>% 
  dplyr::left_join(lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(irr_fields, by = c("UID", "Year"))

## set some criteria for which fields to keep
# within LEMA or buffer
UIDs_keep <- att_fields$UID[att_fields$within_buffer | att_fields$within_lema]
area_m2_thres <- 20000 # 20,000 m2 = 5 acres
ggplot(subset(att_fields, UID %in% UIDs_keep), aes(x = area_m2)) + geom_histogram()

## QA/QC: visualize ET by field 
et_fields