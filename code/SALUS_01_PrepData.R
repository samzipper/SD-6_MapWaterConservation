## SALUS_01_PrepData.R
# Years of overlap are 2016 and 2017

source(file.path("code", "paths+packages.R"))

## years to compare (overlap between SALUS and OpenET are 2016 and 2016)
yrs_comp <- c(2016, 2017)

## field boundaries
sf_fields <- sf::st_read(file.path("data", "Fields_Boundaries.gpkg"))

## SALUS data
SALUS_raw <- readr::read_csv(file.path(dir_data, "SALUS", "SD6_SALUS_and_cropChoice2.csv"),
                             col_types = "ddddddddcdcddcd")
SALUS_trim <- 
  SALUS_raw %>% 
  subset(year %in% yrs_comp &  # overlap with OpenET data
           BestRunFlag_wimas == "LEMA"  # calibrated irrigation thresholds for LEMA
  ) %>% 
  dplyr::select(-BestRunFlag_wimas, -thetaC, -AIrAm)

## OpenET, met, and crop type data
et_fields_yr <- readr::read_csv(file.path("data", "Fields_AnnualET.csv"))
irr_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) %>% 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
lc_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) %>% 
  dplyr::select(-starts_with("pctcov")) %>% 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode")
att_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
met_fields <- readr::read_csv(file.path("data", "gridmet_AnnualByField.csv"))

## combine
fields <-
  dplyr::left_join(irr_fields, lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(att_fields, by = c("UID")) %>% 
  subset(Year %in% yrs_comp & within_lema) %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode") %>% 
  dplyr::left_join(SALUS_trim, by = c("UID", "Year"="year")) %>% 
  subset(is.finite(ET_ann_mm)) %>% 
  dplyr::left_join(met_fields, by = c("UID", "Year"))

## irrigation status compare
fields$IrrCompare <- factor(paste0(fields$Irrigation, "_", fields$AIM_mode_num),
                            levels = c("1_1", "0_1", "1_0", "0_0"),
                            labels = c("1_1" = "Both Irrigated",
                                       "0_1" = "AIM Irrigated",
                                       "1_0" = "Jude Irrigated",
                                       "0_0" = "Both Non-Irrigated"))

table(fields$IrrCompare, fields$Year)

## crop type compare
fields$CDL_mode_group <- fields$CDL_mode_char
fields$CDL_mode_group[fields$CDL_mode_char=="Grass/Pasture"] <- "Grass/Shrub"
fields$CDL_mode_group[fields$CDL_mode_char %in% c("Triticale", "Oats")] <- "Other Small Grains"
fields$CDL_mode_group[fields$CDL_mode_char=="Developed/Open Space"] <- "Developed"
fields$CDL_mode_group[fields$CDL_mode_char=="Alfalfa"] <- "Alfalfa/Hay"
fields$CDL_mode_group[fields$CDL_mode_char=="Barren"] <- "Barren/Water"

table(fields$CropGroup, fields$CDL_mode_group)
sum(fields$CropGroup==fields$CDL_mode_group)
sum(fields$CropGroup==fields$CDL_mode_group)/dim(fields)[1]

# create sf objects for plotting
fields_sf <-
  dplyr::left_join(fields, sf_fields, by = c("UID"))
fields_ET_sf <-
  dplyr::left_join(fields_ET, sf_fields, by = c("UID"))

ggplot(fields_sf, aes(fill = IrrCompare, geometry = geom)) +
  geom_sf(color = NA) +
  facet_wrap(~Year) +
  coord_sf(crs = sf::st_crs(sf_fields)) +
  scale_fill_manual(name = "Irrigation Comparison",
                    values = c("Both Irrigated" = col.cat.blu,
                               "AIM Irrigated" = col.cat.yel,
                               "Jude Irrigated" = col.cat.org,
                               "Both Non-Irrigated" = col.gray)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareIrrigationStatus.png"),
         width = 190, height = 90, units = "mm")

## ET comparison for only fields that agree on irrigation status and crop type
fields_ET <- 
  fields %>% 
  dplyr::left_join(et_fields_yr, ., by = c("UID", "Year")) %>% 
  subset(CDL_mode_group==CropGroup & 
           (IrrCompare == "Both Irrigated" | IrrCompare == "Both Non-Irrigated") &
           Year %in% yrs_comp)

ggplot(fields_ET, aes(x = ET_mm, y = ET_ann_mm, color = CropGroup)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  facet_grid(IrrCompare ~ Algorithm) +
  scale_x_continuous(name = "OpenET Annual ET [mm]") +
  scale_y_continuous(name = "SALUS Annual ET [mm]") +
  scale_color_manual(values = pal_crops) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareET.png"), width = 240, height = 150, units = "mm")
