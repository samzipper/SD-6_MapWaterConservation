## SALUS_Sandbox.R
# Years of overlap are 2016 and 2017

source(file.path("code", "paths+packages.R"))

## years to compare (overlap between SALUS and OpenET are 2016 and 2017)
yrs_comp <- c(2016, 2017)

## field boundaries
sf_fields <- sf::st_read(file.path("data", "Fields_Boundaries.gpkg"))

## load WR groups
# output from script DataPrep_02_WRgroups_SummarizeUse+Fields.R
wrg_fields <- readr::read_csv(file.path("data", "WRgroups_FieldByYear.csv"))
wrg_summary <- readr::read_csv(file.path("data", "WRgroups_UseByWRG.csv"))

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
fields_all <-
  dplyr::left_join(irr_fields, lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(att_fields, by = c("UID")) %>% 
  subset(Year %in% yrs_comp)

fields <- 
  fields_all %>% 
  subset(within_lema) %>% 
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
fields_all_ET <- 
  fields_all %>% 
  dplyr::left_join(et_fields_yr, ., by = c("UID", "Year")) %>% 
  subset(Year %in% yrs_comp)

fields_ET <- 
  fields %>% 
  dplyr::left_join(et_fields_yr, ., by = c("UID", "Year")) %>% 
  subset(Year %in% yrs_comp)

fields_ET %>% 
  subset(CDL_mode_group==CropGroup & 
           (IrrCompare == "Both Irrigated" | IrrCompare == "Both Non-Irrigated") &
           Year %in% yrs_comp &
           CDL_mode_group %in% c("Corn", "Soybeans", "Sorghum", "Winter Wheat")) %>% 
  ggplot(aes(x = ET_mm, y = ET_ann_mm, color = CropGroup)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_grid(IrrCompare ~ Algorithm) +
  scale_x_continuous(name = "OpenET Annual ET [mm]") +
  scale_y_continuous(name = "SALUS Annual ET [mm]") +
  scale_color_manual(name = "Land Cover", values = pal_crops) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareET.png"), width = 240, height = 150, units = "mm")

## summarize non-irrigated ET by crop type
# set some criteria for which fields to keep for estimating non-irrigated ET
area_m2_thres <- 60703 # 60703 m2 = 15 acres

et_nonirr <- 
  fields_all_ET %>%  # for non-irrigated, include buffer area
  subset(Irrigation == 0 & area_m2 > area_m2_thres) %>% 
  dplyr::group_by(Year, Algorithm, CropCode) %>% 
  dplyr::summarize(ET_mm_CropNonirrMedian = median(ET_mm))

## compare SALUS vs OpenET irrigation by field
fields_ET %>% 
  dplyr::left_join(et_nonirr, by = c("Year", "Algorithm", "CropCode")) %>% 
  subset(CDL_mode_group==CropGroup & 
           (IrrCompare == "Both Irrigated") &
           Year %in% yrs_comp &
           CDL_mode_group %in% c("Corn", "Soybeans", "Sorghum", "Winter Wheat")) %>% 
  ggplot(., aes(x = (ET_mm - ET_mm_CropNonirrMedian), y = IRR_ann_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = CropGroup), shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_wrap(~Algorithm) +
  scale_x_continuous(name = "OpenET ET Surplus (ET - non-irrigated ET for same crops) [mm]") +
  scale_y_continuous(name = "SALUS Irrigation [mm]") +
  scale_color_manual(name = "Land Cover", values = pal_crops) +
  labs(title = "SALUS vs. OpenET irrigation depths by field") +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareOpenETByField.png"),
         width = 190, height = 150, units = "mm")

## aggregate to WR group
# first, identify dominant crop type by WR group
crops_wrg <- 
  fields %>% 
  dplyr::left_join(wrg_fields, by = c("Year", "UID")) %>% 
  dplyr::select(UID, Year, area_m2, WR_GROUP, CropCode) %>% 
  unique() %>% 
  dplyr::group_by(Year, WR_GROUP, CropCode) %>% 
  dplyr::summarize(area_m2_crop = sum(area_m2)) %>% 
  dplyr::group_by(Year, WR_GROUP) %>% 
  dplyr::filter(area_m2_crop == max(area_m2_crop)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(area_m2_maincrop = area_m2_crop,
                CropCode_maincrop = CropCode) %>% 
  dplyr::left_join(crop_names.groups, by = c("CropCode_maincrop" = "CropCode"))

# now, summarize other characteristics
alldata_wrg <- 
  fields_ET %>% 
  dplyr::left_join(wrg_fields, by = c("Year", "UID")) %>% 
  dplyr::left_join(et_nonirr, by = c("CropCode", "Year", "Algorithm")) %>% 
  subset(is.finite(WR_GROUP)) %>%  # non-irrigated fields have NA for WR_GROUP
  dplyr::group_by(WR_GROUP, Year, Algorithm) %>% 
  dplyr::summarize(n_irrfields = length(unique(UID)), # all fields are irrigated, so this will be equal to # of irrigated fields
                   area_m2_wrg = round(sum(area_m2), 2),
                   n_within_lema = sum(within_lema),
                   n_within_buffer = sum(within_buffer),
                   n_croptypes = length(unique(CropCode)),
                   IRR_wrg_m3 = round(sum(IRR_ann_mm*area_m2)/1000, 2),
                   RCH_wrg_m3 = round(sum(RCH_ann_mm*area_m2)/1000, 2),
                   PPT_wrg_m3 = round(sum(PPT_ann_mm*area_m2)/1000, 2),
                   ET_m3 = round(sum((ET_mm/1000)*area_m2), 2),
                   ET_m3_nonirr = round(sum((ET_mm_CropNonirrMedian/1000)*area_m2), 2),
                   ETo_m3 = round(sum((ETo_mm/1000)*area_m2), 2),
                   ETr_m3 = round(sum((ETr_mm/1000)*area_m2), 2),
                   precip_m3 = round(sum((precip_mm/1000)*area_m2), 2)) %>% 
  dplyr::left_join(crops_wrg, by = c("Year", "WR_GROUP")) %>% 
  dplyr::mutate(area_prc_maincrop = area_m2_maincrop/area_m2_wrg) %>% 
  dplyr::left_join(wrg_summary, by = c("Year", "WR_GROUP")) %>% 
  subset(n_within_lema == n_irrfields)

### plot water rights groups
# SALUS vs WIMAS - does not use OpenET so subset to a single algorithm
ggplot(subset(alldata_wrg, Algorithm == "ensemble"), aes(x = Irrigation_m3/1e+05, y = IRR_wrg_m3/1e+05)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]") +
  scale_y_continuous(name = "SALUS Irrigation [x10\u2075 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  labs(title = "SALUS vs. WIMAS irrigation volumes by water rights group") +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareWIMASByWRG.png"),
         width = 120, height = 120, units = "mm")

# SALUS vs OpenET
ggplot(alldata_wrg, aes(x = (ET_m3 - ET_m3_nonirr)/1e+05, y = IRR_wrg_m3/1e+05)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  facet_wrap(~Algorithm) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "OpenET ET Surplus (ET - non-irrigated ET for same crops) [x10\u2075 m\u00b3]") +
  scale_y_continuous(name = "SALUS Irrigation [x10\u2075 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  labs(title = "SALUS vs. OpenET irrigation volumes by water rights group") +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareOpenETByWRG.png"),
         width = 190, height = 150, units = "mm")

# OpenET vs WIMAS
ggplot(alldata_wrg, aes(x = Irrigation_m3/1e+05, y = (ET_m3 - ET_m3_nonirr)/1e+05)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  facet_wrap(~Algorithm) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]") +
  scale_y_continuous(name = "OpenET ET Surplus (ET - non-irrigated ET for same crops) [x10\u2075 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  labs(title = "OpenET vs. WIMAS irrigation volumes by water rights group") +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUS_CompareWIMAS-OpenETByWRG.png"),
         width = 190, height = 150, units = "mm")

## focus in on eeMETRIC
# METRIC vs SALUS, irrigated fields only
fields_ET %>% 
  subset(Algorithm == "eemetric" &
           CDL_mode_group==CropGroup & 
           IrrCompare == "Both Irrigated" &
           Year %in% yrs_comp &
           CDL_mode_group %in% c("Corn", "Soybeans", "Sorghum", "Winter Wheat")) %>% 
  ggplot(aes(y = ET_mm, x = ET_ann_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  scale_y_continuous(name = "eeMETRIC Annual ET [mm]") +
  scale_x_continuous(name = "SALUS Annual ET [mm]") +
  scale_color_manual(name = NULL, values = pal_crops) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUSvMETRIC_CompareET.png"), width = 120, height = 120, units = "mm")

fields_ET %>% 
  subset(Algorithm == "eemetric" &
           CDL_mode_group==CropGroup & 
           IrrCompare == "Both Irrigated" &
           Year %in% yrs_comp &
           CDL_mode_group %in% c("Corn", "Soybeans", "Sorghum", "Winter Wheat")) %>% 
  ggplot(aes(y = ET_mm, x = (PPT_ann_mm + IRR_ann_mm))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  scale_y_continuous(name = "eeMETRIC Annual ET [mm]") +
  scale_x_continuous(name = "SALUS Annual Irrigation + Precip [mm]") +
  scale_color_manual(name = NULL, values = pal_crops) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUSvMETRIC_CompareIrr+Prec.png"), width = 120, height = 120, units = "mm")

fields_ET %>% 
  subset(Algorithm == "eemetric" &
           CDL_mode_group==CropGroup & 
           IrrCompare == "Both Irrigated" &
           Year %in% yrs_comp &
           CDL_mode_group %in% c("Corn", "Soybeans", "Sorghum", "Winter Wheat")) %>% 
  ggplot(aes(y = ET_mm, x = (PPT_ann_mm + IRR_ann_mm - RCH_ann_mm))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  scale_y_continuous(name = "eeMETRIC Annual ET [mm]") +
  scale_x_continuous(name = "SALUS Annual Irrigation + Precip - Deep Perc [mm]") +
  scale_color_manual(name = NULL, values = pal_crops) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUSvMETRIC_CompareIrr+Prec-Perc.png"), width = 120, height = 120, units = "mm")

fields_ET %>% 
  subset(Algorithm == "eemetric" &
           CDL_mode_group==CropGroup & 
           IrrCompare == "Both Irrigated" &
           Year %in% yrs_comp &
           CDL_mode_group %in% c("Corn", "Soybeans", "Sorghum", "Winter Wheat")) %>% 
  ggplot(aes(y = ET_ann_mm, x = (PPT_ann_mm + IRR_ann_mm - RCH_ann_mm))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  scale_y_continuous(name = "SALUS Annual ET [mm]") +
  scale_x_continuous(name = "SALUS Annual Irrigation + Precip - Deep Perc [mm]") +
  scale_color_manual(name = NULL, values = pal_crops) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "SALUSvSALUS_CompareIrr+Prec-Perc.png"), width = 120, height = 120, units = "mm")

# water rights groups volumes - eeMETRIC vs WIMAS
# ET surplus
ggplot(subset(alldata_wrg, Algorithm == "eemetric"), 
       aes(x = Irrigation_m3/1e+05, y = (ET_m3 - ET_m3_nonirr)/1e+05)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]") +
  scale_y_continuous(name = "eeMETRIC ET Surplus (ET - non-irrigated ET for same crops) [x10\u2075 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "WIMASvMETRIC_CompareWIMAS-METRICsurplusByWRG.png"),
         width = 120, height = 120, units = "mm")

# ET - precip
ggplot(subset(alldata_wrg, Algorithm == "eemetric"), 
       aes(x = Irrigation_m3/1e+05, y = (ET_m3-precip_m3)/1e+05)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]") +
  scale_y_continuous(name = "eeMETRIC ET - Precip [x10\u2075 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "WIMASvMETRIC_CompareWIMAS-METRICetDeficitByWRG.png"),
         width = 120, height = 120, units = "mm")

## water rights groups depths - eeMETRIC vs WIMAS
# ET surplus
ggplot(subset(alldata_wrg, Algorithm == "eemetric"), 
       aes(x = 1000*Irrigation_m3/area_m2_wrg, y = 1000*(ET_m3 - ET_m3_nonirr)/area_m2_wrg)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Reported Irrigation [mm]") +
  scale_y_continuous(name = "eeMETRIC ET Surplus (ET - non-irrigated ET for same crops) [mm]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "WIMASvMETRICdepth_CompareWIMAS-METRICsurplusByWRG.png"),
         width = 120, height = 120, units = "mm")

# ET - precip
ggplot(subset(alldata_wrg, Algorithm == "eemetric"), 
       aes(x = 1000*Irrigation_m3/area_m2_wrg, y = 1000*(ET_m3-precip_m3)/area_m2_wrg)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year))) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Reported Irrigation [mm]") +
  scale_y_continuous(name = "eeMETRIC ET - Precip [mm]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.grn)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "WIMASvMETRICdepth_CompareWIMAS-METRICetDeficitByWRG.png"),
         width = 120, height = 120, units = "mm")
