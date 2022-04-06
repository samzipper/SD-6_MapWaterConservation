## OpenET_04_CompareToSALUS.R

source(file.path("code", "paths+packages.R"))

# load data
fields_openet <- read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "OpenET_EstimateFieldIrrigation_FieldsNoDups.csv"))
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

SALUS_raw <- readr::read_csv(file.path(dir_data, "SALUS", "SD6_SALUS_and_cropChoice2.csv"),
                             col_types = "ddddddddcdcddcd")
SALUS_trim <- 
  SALUS_raw %>% 
  subset(year %in% fields_openet$Year &  # overlap with OpenET data
           BestRunFlag_wimas == "LEMA"  # calibrated irrigation thresholds for LEMA
  ) %>% 
  dplyr::select(-BestRunFlag_wimas, -thetaC, -AIrAm)

## crop type compare
SALUS_trim$CDL_mode_group <- SALUS_trim$CDL_mode_char
SALUS_trim$CDL_mode_group[SALUS_trim$CDL_mode_char=="Grass/Pasture"] <- "Grass/Shrub"
SALUS_trim$CDL_mode_group[SALUS_trim$CDL_mode_char %in% c("Triticale", "Oats")] <- "Other Small Grains"
SALUS_trim$CDL_mode_group[SALUS_trim$CDL_mode_char=="Developed/Open Space"] <- "Developed"
SALUS_trim$CDL_mode_group[SALUS_trim$CDL_mode_char=="Alfalfa"] <- "Alfalfa/Hay"
SALUS_trim$CDL_mode_group[SALUS_trim$CDL_mode_char=="Barren"] <- "Barren/Water"

# combine all data
alldata_fields <- 
  fields_openet %>% 
  dplyr::left_join(fields_spatial, by = "UID") %>% 
  dplyr::left_join(fields_irrigation, by = c("UID", "Year")) %>% 
  dplyr::left_join(fields_landcover, by = c("UID", "Year")) %>% 
  dplyr::left_join(SALUS_trim, by = c("UID", "Year"="year"))

# subset to final data frame for comparison
compare_fields <-
  alldata_fields %>% 
  subset(within_lema) %>% # within LEMA only
  subset(is.finite(ET_ann_mm)) %>%   # finite SALUS data (only years where SALUS was run)
  #subset(area_m2 > 40468) %>%  # 40468 m2 = 10 acres
  subset(Irrigation == AIM_mode_num) %>%  # agree on irrigation status
  subset(CropGroup == CDL_mode_group) %>%  # agree on crop type
  subset(CropGroup %in% c("Corn", "Sorghum", "Soybeans"))  # crop types that are common in both irrigated and non-irrigated land)

# compare irrigation and ET
p_irr_compare <-
  ggplot(subset(compare_fields, Irrigation == 1), aes(x = irr_mm, y = IRR_ann_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) + 
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = 150, ymax = 300, color = NA, fill = col.cat.blu, alpha = 0.25) +
  #annotate("rect", ymin = -Inf, ymax = Inf, xmin = 150, xmax = 300, color = NA, fill = col.cat.blu, alpha = 0.25) +
  geom_point(aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  facet_grid(Year ~ Algorithm, labeller = as_labeller(c(labs_algorithms, "2016" = "2016", "2017" = "2017"))) +
  scale_x_continuous(name = "OpenET Irrigation [mm]", breaks = seq(0, 600, 300)) +
  scale_y_continuous(name = "SALUS Irrigation [mm]") +
  scale_color_manual(name = "Crop", values = pal_crops[1:3], drop = TRUE) +
  #coord_equal() +
  theme(legend.position = "bottom") +
  #labs(title = "Comparison of SALUS and OpenET Field-Resolution Irrigation Depth",
  #     subtitle = "Subset to: LEMA, irrigated fields, 3 most common crops") +
  NULL
#ggsave(file.path("plots", "OpenET_04_CompareToSALUS_AnnualIrr.png"),
#      width = 280, height = 100, units = "mm")


p_ET_compare <-
  ggplot(compare_fields, aes(x = ET_mm, y = ET_ann_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) + 
  geom_point(aes(color = CropGroup, shape = Irrigation==1)) +
  stat_smooth(method = "lm") +
  facet_grid(Year ~ Algorithm, labeller = as_labeller(c(labs_algorithms, "2016" = "2016", "2017" = "2017"))) +
  scale_x_continuous(name = "OpenET ET [mm]", breaks = seq(400, 1200, 400)) +
  scale_y_continuous(name = "SALUS ET [mm]", breaks = seq(400, 1200, 200)) +
  scale_color_manual(name = "Crop", values = pal_crops[1:3], drop = TRUE) +
  scale_shape_manual(name = "Irrigation Status", values = c("TRUE" = 16, "FALSE" = 1), 
                     labels = c("TRUE" = "Irrigated", "FALSE" = "Non-Irrigated")) +
  #coord_equal() +
  theme(legend.position = "bottom") +
  #labs(title = "Comparison of SALUS and OpenET Field-Resolution ET Depth",
  #     subtitle = "Subset to: LEMA, 3 most common crops") +
  NULL
#ggsave(file.path("plots", "OpenET_04_CompareToSALUS_AnnualET.png"),
#       width = 280, height = 100, units = "mm")

p_combo <-
  (p_ET_compare + p_irr_compare) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
p_combo

ggsave(file.path("plots", "OpenET_04_CompareToSALUS.png"),
       p_combo, width = 190, height = 120, units = "mm")

# compare RMSE for irrigation and ET
rsq <- function(x, y) summary(lm(y~x))$r.squared
summary_fit <-
  compare_fields %>% 
  subset(Irrigation == 1) %>% 
  group_by(Algorithm) %>% 
  summarize(et_rmsd = hydroGOF::rmse(ET_ann_mm, ET_mm),
            irr_rmsd = hydroGOF::rmse(IRR_ann_mm, irr_mm),
            et_r2 = rsq(ET_ann_mm, ET_mm),
            irr_r2 = rsq(IRR_ann_mm, irr_mm))

ggplot(summary_fit, aes(x = et_rmsd, y = irr_rmsd)) +
  geom_point(color = "red") +
  geom_text(aes(label = Algorithm)) +
  labs(title = "Root mean squared difference from SALUS",
       subtitle = "Subset to: LEMA, irrigated fields, 3 most common crops")
ggsave(file.path("plots", "OpenET_04_CompareToSALUS_FitRMSDbyModel.png"),
       width = 95, height = 95, units = "mm")

ggplot(summary_fit, aes(x = et_r2, y = irr_r2)) +
  geom_point(color = "red") +
  geom_text(aes(label = Algorithm)) +
  labs(title = "R^2 with SALUS",
       subtitle = "Subset to: LEMA, irrigated fields, 3 most common crops")
ggsave(file.path("plots", "OpenET_04_CompareToSALUS_FitR2byModel.png"),
       width = 95, height = 95, units = "mm")
