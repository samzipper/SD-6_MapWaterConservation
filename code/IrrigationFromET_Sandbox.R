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
  ggsave(file.path("plots", "IrrigationFromET_ETrateBoxplots.png"),
         width = 190, height = 190, units = "mm")

## add non-irrigated ET to WRG data
wrg_with_et <-
  alldata_wrg %>% 
  dplyr::left_join(crop_names.groups, by = c("CropCode_maincrop" = "CropCode")) %>% 
  dplyr::left_join(et_nonirr, by = "CropGroup")

  