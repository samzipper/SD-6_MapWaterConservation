## OpenET_03_EstimateFieldIrrigation.R
# This script will estimate irrigation at the field level.

source(file.path("code", "paths+packages.R"))

## load data
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

# ET rates
et_fields_yr <- 
  file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Annual_All_FieldsNoDups.csv") %>% 
  read_csv() %>% 
  subset(Year <= 2020)

et_fields_mo <-
  file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Monthly_All_FieldsNoDups.csv") %>% 
  read_csv() %>% 
  mutate(Year = year(Date)) %>% 
  subset(Year <= 2020)

# run some checks on irrigation status
et_fields_mo_irr <-
  et_fields_mo %>% 
  left_join(fields_irrigation, by = c("UID", "Year")) %>% 
  left_join(fields_landcover, by = c("UID", "Year"))

# inspect a single crop/year combo
et_mo_cropyr <- subset(et_fields_mo_irr, Year == 2017 & CropGroupCoarse == "Corn")
et_pk_cropyr <- 
  et_mo_cropyr %>% 
  group_by(UID) %>% 
  filter(ET_mm_mean_ensemble == max(ET_mm_mean_ensemble))

ggplot(et_mo_cropyr, aes(x = Date, y = ET_mm_mean_ensemble, color = (Irrigation == 1), group = UID)) +
  geom_line()

ggplot(et_pk_cropyr, aes(x = (Irrigation == 1), y = ET_mm_mean_ensemble)) +
  geom_boxplot()

# define a crop/irrigation confidence variable
#  - low  = for irrigated, peak ET < 75th percentile of non-irrigated ET
#           for non-irrigated, peak ET > 75th percentile of irrigated ET
#  - high = for irrigated, peak ET > 75th percentile of non-irrigated ET
#           for non-irrigtaed, peak ET < 75th percentile of irrigated ET
et_pk_75p_irr <- quantile(subset(et_pk_cropyr, Irrigation == 1)$ET_mm_mean_ensemble, 0.75)
et_pk_75p_nonirr <- quantile(subset(et_pk_cropyr, Irrigation == 0)$ET_mm_mean_ensemble, 0.75)



## get all data together
fields_alldata <-
  left_join(et_fields_yr, met_yearly_fields, by = c("Year", "UID")) %>% 
  left_join(fields_spatial, by = "UID") %>% 
  left_join(fields_irrigation, by = c("Year", "UID")) %>% 
  left_join(fields_landcover, by = c("Year", "UID")) %>% 
  mutate(irr_mm = round(ET_mm - precip_mm, 3),
         irr_m3 = round((irr_mm/1000)*area_m2, 1))

# find UIDs where we want to set irrigation = 0 across all years because 
# they are in CropGroup that is not irrigated (assume that it doesn't change for future year)
cropgroup_nonirr <- c("Wetland", "Forest", "Developed", "Barren/Water")
UIDs_0_landcover <- unique(fields_alldata$UID[fields_alldata$CropGroup %in% cropgroup_nonirr])

fields_alldata$irr_m3[fields_alldata$UID %in% UIDs_0_landcover] <- 0
fields_alldata$irr_mm[fields_alldata$UID %in% UIDs_0_landcover] <- 0

# negative irrigation is impossible: set all negative values to 0
fields_alldata$irr_m3[fields_alldata$irr_m3 < 0] <- 0
fields_alldata$irr_mm[fields_alldata$irr_mm < 0] <- 0

## select desired output and save
fields_openet <-
  fields_alldata %>% 
  dplyr::select(UID, Year, Algorithm, ET_mm, precip_mm, irr_mm, irr_m3)

write_csv(fields_openet, file.path(dir_data, "OpenET", "Monthly_2016-2021", "OpenET_EstimateFieldIrrigation_FieldsNoDups.csv"))
