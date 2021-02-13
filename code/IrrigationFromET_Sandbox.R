## IrrigationFromET_Sandbox.R
# Estimate irrigation from ET.

source(file.path("code", "paths+packages.R"))

## load data
# annual ET and wr group summaries (from DataPrep_03_WRgroups_SummarizeAnnualData.R)
alldata_wrg <- readr::read_csv(file.path("data", "WRgroups_AnnualData.csv")) %>% 
  dplyr::mutate(ET_mm = ET_m3/area_m2_wrg,
                ET_m3_surplus = ET_m3 - ET_m3_nonirr,
                ET_mm_surplus = ET_m3_surplus/area_m2_wrg) %>% 
  dplyr::left_join(crop_names.groups, by = c("CropCode_maincrop" = "CropCode"))
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
  dplyr::left_join(irr_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode")

## set some criteria for which fields to keep
UIDs_keep <- att_fields$UID[att_fields$within_buffer | att_fields$within_lema]  # within LEMA or buffer
CropGroup_keep <- c("Corn", "Sorghum", "Soybeans")  # crop types that are common in both irrigated and non-irrigated land
area_m2_thres <- 60703 # 60703 m2 = 15 acres

## summarize non-irrigated ET for crop groups of interest
et_nonirr <- 
  alldata_fields %>% 
  subset(UID %in% UIDs_keep & CropGroup %in% CropGroup_keep & Irrigation == 0) %>% 
  dplyr::group_by(Year, Algorithm, CropGroup) %>% 
  dplyr::summarize(ET_mm_crop_nonirr_mean = mean(ET_mm),
                   ET_mm_crop_nonirr_median = median(ET_mm))

alldata_fields %>% 
  subset(UID %in% UIDs_keep & CropGroup %in% CropGroup_keep) %>% 
  ggplot(aes(x = Algorithm, y = ET_mm, fill = factor(Irrigation))) +
  geom_boxplot() +
  facet_grid(Year ~ CropGroup) +
  scale_x_discrete(name = "ET Algorithm") +
  scale_y_continuous(name = "Mean ET [mm]") +
  scale_fill_manual(name = "Irrigation Status", 
                    values = c("0" = col.gray, "1" = col.cat.blu),
                    labels = c("0" = "Non-Irrigated", "1" = "Irrigated")) +
  labs(title = "ET rates for dominant crops (LEMA and buffer area)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggsave(file.path("plots", "IrrigationFromET_ETrateIrrNonirr.png"),
         width = 190, height = 190, units = "mm")

## plot witihn vs outside LEMA ET rates
alldata_fields %>% 
  subset(UID %in% UIDs_keep & CropGroup %in% CropGroup_keep & Irrigation == 1) %>% 
  ggplot(aes(x = Algorithm, y = ET_mm, fill = factor(within_lema))) +
  geom_boxplot() +
  facet_grid(Year ~ CropGroup) +
  scale_x_discrete(name = "ET Algorithm") +
  scale_y_continuous(name = "Mean ET [mm]") +
  scale_fill_manual(name = "Location", 
                    values = c("FALSE" = col.cat.red, "TRUE" = col.cat.blu),
                    labels = c("FALSE" = "Buffer", "TRUE" = "LEMA")) +
  labs(title = "Field-resolution irrigated ET rates for dominant crops") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggsave(file.path("plots", "IrrigationFromET_IrrigatedETrateByCrop.png"),
         width = 190, height = 190, units = "mm")

## compare by water rights group
# entirely in LEMA or buffer
wrg_lema <- unique(alldata_wrg$WR_GROUP[alldata_wrg$n_within_lema == alldata_wrg$n_irrfields])
wrg_buffer <- unique(alldata_wrg$WR_GROUP[alldata_wrg$n_within_buffer == alldata_wrg$n_irrfields])

# comparable area between WIMAS and inferred groups
alldata_wrg$area_diff_prc <- (alldata_wrg$IrrArea_m2 - alldata_wrg$area_m2_wrg)/alldata_wrg$IrrArea_m2

# crop types that are common in both irrigated and non-irrigated land
CropGroup_keep <- c("Corn", "Sorghum", "Soybeans")  


trimmed_wrg <- 
  alldata_wrg %>% 
  subset(WR_GROUP %in% c(wrg_lema, wrg_buffer)) %>% 
  subset(CropGroup %in% CropGroup_keep) %>% 
  subset(abs(area_diff_prc) > 0.2) %>% 
  subset(area_m2_maincrop/area_m2_wrg > 0.8)

ggplot(trimmed_wrg, aes(x = Irrigation_m3, y = ET_m3_surplus,
                        color = CropGroup)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  scale_color_manual(values = pal_crops)

ggplot(trimmed_wrg, aes(x = CropGroup, y = ET_mm_surplus,
                        fill = (WR_GROUP %in% wrg_lema))) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot() +
  facet_wrap(~ Algorithm, scales = "free") +
  scale_x_discrete(name = "Main Crop Type") +
  scale_y_continuous(name = "Annual ET Surplus [mm]") +
  scale_fill_manual(name = "Location", 
                    values = c("FALSE" = col.cat.red, "TRUE" = col.cat.blu),
                    labels = c("FALSE" = "Buffer", "TRUE" = "LEMA")) +
  labs(title = "ET surplus (actual ET - median nonirrigated ET for that crop)",
       subtitle = "2016-2018 data, trimmed water rights groups") +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "IrrigationFromET_ETsurplusByAlgorithm.png"),
         width = 190, height = 120, units = "mm")
