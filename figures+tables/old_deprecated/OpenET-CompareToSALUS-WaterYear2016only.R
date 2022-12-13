## OpenET-CompareToSALUS-WaterYear2016only.R
# Create figure comparing OpenET-based estimates of irrigation and SALUS-based estimates.

source(file.path("code", "paths+packages.R"))

# load data
fields_openet <- read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "OpenET_EstimateFieldIrrigation-WaterYear_FieldsNoDups.csv"))
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) |> 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
fields_landcover <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) |> 
  dplyr::select(-starts_with("pctcov")) |> 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode") |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

SALUS_raw <- readr::read_csv(file.path(dir_data, "SALUS", "SD6_SALUS_and_cropChoice2.csv"),
                             col_types = "ddddddddcdcddcd")
SALUS_trim <- 
  SALUS_raw |> 
  subset(year %in% fields_openet$WaterYear &  # overlap with OpenET data
           BestRunFlag_wimas == "LEMA"  # calibrated irrigation thresholds for LEMA
  ) |> 
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
  fields_openet |> 
  dplyr::left_join(fields_spatial, by = "UID") |> 
  dplyr::left_join(fields_irrigation, by = c("UID", "WaterYear"="Year")) |> 
  dplyr::left_join(fields_landcover, by = c("UID", "WaterYear"="Year")) |> 
  dplyr::left_join(SALUS_trim, by = c("UID", "WaterYear"="year"))

# subset to final data frame for comparison
compare_fields <-
  alldata_fields |> 
  subset(WaterYear == 2016) |> # 2016 comparison only due to precipitation differences in 2017
  subset(within_lema) |> # within LEMA only
  subset(is.finite(ET_ann_mm)) |>   # finite SALUS data (only years where SALUS was run)
  #subset(area_m2 > 40468) |>  # 40468 m2 = 10 acres
  subset(Irrigation == AIM_mode_num) |>  # agree on irrigation status
  subset(CropGroup == CDL_mode_group) |>  # agree on crop type
  subset(CropGroup %in% c("Corn", "Sorghum", "Soybeans")) |>  # crop types that are common in both irrigated and non-irrigated land)
  subset(IrrConfidence == "High")  # high degree of confidence in irrigation status

## compare irrigation and ET
# irrigation comparison
compare_fields_irr <- subset(compare_fields, Irrigation == 1)
p_irr_min <- min(c(compare_fields_irr$irr_mm_fromPrec, compare_fields_irr$irr_mm_fromNonIrr, compare_fields_irr$IRR_ann_mm))
p_irr_max <- max(c(compare_fields_irr$irr_mm_fromPrec, compare_fields_irr$irr_mm_fromNonIrr, compare_fields_irr$IRR_ann_mm))
p_irr_limits <- c(floor(p_irr_min), ceiling(p_irr_max))
p_irr_breaks <- seq(0, 600, 150)

p_irr_fromPrec_compare <-
  ggplot(compare_fields_irr, aes(x = irr_mm_fromPrec, y = IRR_ann_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) + 
  geom_point(aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  facet_wrap(. ~ Algorithm, 
             labeller = as_labeller(c(labs_algorithms, "2016" = "2016")),
             nrow = 2) +
  scale_x_continuous(name = "OpenET ET - Precip [mm]", limits = p_irr_limits, breaks = p_irr_breaks) +
  scale_y_continuous(name = "SALUS Irrigation [mm]", limits = p_irr_limits, breaks = p_irr_breaks) +
  scale_color_manual(name = "Crop", values = pal_crops[1:3], drop = TRUE) +
  #coord_equal() +
  theme(legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "OpenET-CompareToSALUS-WaterYear2016only_IrrigationComparison-FromPrecOnly.png"),
       p_irr_fromPrec_compare, width = 17.15, height = 9.5, units = "cm")

p_irr_fromNonIrr_compare <-
  ggplot(compare_fields_irr, aes(x = irr_mm_fromNonIrr, y = IRR_ann_mm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) + 
  geom_point(aes(color = CropGroup)) +
  stat_smooth(method = "lm") +
  facet_wrap(. ~ Algorithm, 
             labeller = as_labeller(c(labs_algorithms, "2016" = "2016")),
             nrow = 2) +
  scale_x_continuous(name = "OpenET ET - Nonirrigated ET [mm]", limits = p_irr_limits, breaks = p_irr_breaks) +
  scale_y_continuous(name = "SALUS Irrigation [mm]", limits = p_irr_limits, breaks = p_irr_breaks) +
  scale_color_manual(name = "Crop", values = pal_crops[1:3], drop = TRUE) +
  #coord_equal() +
  theme(legend.position = "bottom") +
  NULL

p_combo <-
  ((p_irr_fromPrec_compare + labs(title = "(a) Precipitation-based Irrigation Estimate")) + 
     (p_irr_fromNonIrr_compare + labs(title = "(b) Nonirrigated Crop-based Irrigation Estimate"))) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
p_combo

ggsave(file.path("figures+tables", "OpenET-CompareToSALUS-WaterYear2016only_IrrigationComparison.png"),
       p_combo, width = 17.15, height = 17.15, units = "cm")

# ET comparison not valid since we only have annual ET