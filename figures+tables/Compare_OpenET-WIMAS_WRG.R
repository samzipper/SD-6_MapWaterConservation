# Compare_OpenET-WIMAS_WRG.R

source(file.path("code", "paths+packages.R"))

# timescale for OpenET data
ts <- "GrowingSeason"

# load WRG data
wrg_use <- 
  read_csv(file.path("data", "WRGs_UseByWRG.csv")) |> 
  mutate(irrFieldArea_m2 = LEMA_irrFieldArea_m2 + notLEMA_irrFieldArea_m2) |> 
  subset(Year <= 2020)

# identify NAs - these can occur when a WRG has no fields that are mapped as irrigated
summary(wrg_use)
wrg_use_trim <-
  wrg_use |> 
  subset(is.finite(LEMA_irrFieldArea_fraction))

# set threshold of irrigated area agreement for "good fits"
prc_thres <- 0.1  # 10%

# identify good fits
wrg_use_trim$irrArea_pctDiff <- (wrg_use_trim$irrFieldArea_m2 - wrg_use_trim$WRGirrAreaReported_m2)/wrg_use_trim$WRGirrAreaReported_m2
wrg_use_trim$irrArea_goodFit <- abs(wrg_use_trim$irrArea_pctDiff) <= prc_thres

# load OpenET estimates
df_irr <- 
  read_csv(file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv")))

# load field for each WRG and irrigation for each field
wrg_fields <- 
  read_csv(file.path("data", "WRGs_WRGbyField.csv"))

# link reported and OpenET irrigation
df_wrg_irr_all <-
  df_irr |> 
  left_join(wrg_fields, by = "UID") |> 
  group_by(WR_GROUP, Year, Algorithm) |> 
  summarize(WRGirrigationTotal_m3 = sum(FieldIrrigation_m3)) |> 
  left_join(wrg_use_trim, by = c("Year", "WR_GROUP"), suffix = c("_OpenET", "_Reported")) |> 
  subset(is.finite(WRGirrigationTotal_m3_Reported))

# calculate irrigation depths
df_wrg_irr_all$WRGirrigationTotal_mm_Reported <- 1000*df_wrg_irr_all$WRGirrigationTotal_m3_Reported/df_wrg_irr_all$WRGirrAreaReported_m2
df_wrg_irr_all$WRGirrigationTotal_mm_OpenET <- 1000*df_wrg_irr_all$WRGirrigationTotal_m3_OpenET/df_wrg_irr_all$irrFieldArea_m2

# Irrigated area comparison -----------------------------------------------

n_good <- sum(wrg_use_trim$irrArea_goodFit)
n_total <- length(wrg_use_trim$irrArea_goodFit)
print(paste0(n_good, " of ", n_total, " good (", 100*round(n_good/n_total, 2), "%)"))

## plot - comparison of irrigated area
p_irrArea_compare <-
  ggplot(wrg_use_trim, aes(x = irrFieldArea_m2/1e4, y = WRGirrAreaReported_m2/1e4)) +
  geom_point(aes(shape = irrArea_goodFit, color = irrArea_goodFit)) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  scale_x_continuous(name = "Irrigated Area, Sum of Fields [ha]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use_trim$irrFieldArea_m2/1e4, na.rm = T))) +
  scale_y_continuous(name = "Irrigated Area, Reported [ha]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use_trim$irrFieldArea_m2/1e4, na.rm = T))) +
  coord_equal() +
  scale_shape_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = 1, "TRUE" = 16)) +
  scale_color_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.red)) +
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(1,0))

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrArea_AllWRGs.png"),
       p_irrArea_compare, width = 95, height = 95, units = "mm")

# Volume comparison - area agreement only ---------------------------------

# subset to good area match only
df_wrg_irr_areaMatch <-
  df_wrg_irr_all |> 
  subset(irrArea_goodFit)

# plot
p_wrg_fit <-
  ggplot(df_wrg_irr_areaMatch, aes(x = WRGirrigationTotal_m3_OpenET/1e5, 
                                   y = WRGirrigationTotal_m3_Reported/1e5)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = factor(Year)), shape = 1) +
  facet_wrap( ~ Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr_areaMatch$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr_areaMatch$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL
ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrVolume_AllWRGs-AreaMatch.png"),
       p_wrg_fit, width = 190, height = 110, units = "mm")

# calculate fit stats
getR2 <- function(y, x) summary(lm(y~x))$r.squared
getSlope <- function(y, x) coefficients(lm(y~x))[2]
df_fit_long <-
  df_wrg_irr_areaMatch |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            slope = getSlope(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported))

write_csv(df_fit_long, file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_FitStats_AllWRGs-AreaMatch.csv"))

# Volume comparison - simple WRGs only ------------------------------------

## subset to simple WRGs - 1 PDIV, 1 water right
# find simple WR_GROUPs
wrg_nfields <-
  wrg_fields |> 
  group_by(WR_GROUP) |> 
  summarize(n_fields = length(unique(UID)))

wrg_npdiv <-
  st_read(file.path("data", "WRGs_PDIVbyWRG.gpkg")) |> 
  st_drop_geometry() |> 
  dplyr::select(-PDIV_ID, -UMW_CODE) |> 
  unique()

wrg_simple <- wrg_npdiv$WR_GROUP[wrg_npdiv$WRG_nWaterRight == 1 & wrg_npdiv$WRG_nPDIV == 1]

# compare reported and inferred irrigated area
wrg_use_simple <- subset(wrg_use_trim, WR_GROUP %in% wrg_simple)

n_simple_good <- sum(wrg_use_simple$irrArea_goodFit)
n_simple_total <- length(wrg_use_simple$irrArea_goodFit)
print(paste0(n_simple_good, " of ", n_simple_total, " good (", 100*round(n_simple_good/n_simple_total, 2), "%)"))

# plot - comparison of irrigated area
p_irrArea_compare_simple <-
  ggplot(wrg_use_simple, aes(x = irrFieldArea_m2/1e4, y = WRGirrAreaReported_m2/1e4)) +
  geom_point(aes(shape = irrArea_goodFit, color = irrArea_goodFit)) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  scale_x_continuous(name = "Irrigated Area, Sum of Fields [ha]",
                     expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, max(wrg_use_simple$irrFieldArea_m2/1e4, na.rm = T))) +
  scale_y_continuous(name = "Irrigated Area, Reported [ha]",
                     expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, max(wrg_use_simple$irrFieldArea_m2/1e4, na.rm = T))) +
  coord_equal() +
  scale_shape_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = 1, "TRUE" = 16)) +
  scale_color_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.red)) +
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(1,0))

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrArea_SimpleWRGs.png"),
       p_irrArea_compare_simple, width = 95, height = 95, units = "mm")

# plot - comparison of irrigated volume
# subset use to only simple WRGs
df_wrg_irr_areaMatch_simple <-
  df_wrg_irr_areaMatch |> 
  subset(WR_GROUP %in% wrg_simple)

p_wrg_fit_simple <-
  ggplot(df_wrg_irr_areaMatch_simple, aes(x = WRGirrigationTotal_m3_OpenET/1e5, 
                                          y = WRGirrigationTotal_m3_Reported/1e5)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = factor(Year)), shape = 1) +
  facet_wrap( ~ Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr_areaMatch_simple$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr_areaMatch_simple$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL
ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrVolume_SimpleWRGs-AreaMatch.png"),
       p_wrg_fit_simple, width = 190, height = 110, units = "mm")

# calculate fit stats
df_fit_simple_long <-
  df_wrg_irr_areaMatch_simple |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            slope = getSlope(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported))

write_csv(df_fit_simple_long, file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_FitStats_SimpleWRGs-AreaMatch.csv"))

# Volume comparison - all WRGs --------------------------------------------

p_wrg_vol_all <-
  ggplot(df_wrg_irr_all, aes(x = WRGirrigationTotal_m3_OpenET/1e5, 
                           y = WRGirrigationTotal_m3_Reported/1e5)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = irrArea_goodFit), shape = 1) +
  facet_wrap( ~ Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr_all$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr_all$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Area Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.red)) +  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrVolume_AllWRGs.png"),
       p_wrg_vol_all, width = 190, height = 110, units = "mm")

# Depth comparison - all WRGs ---------------------------------------------

p_wrg_depth_all <-
  ggplot(df_wrg_irr_all, aes(x = WRGirrigationTotal_mm_OpenET, 
                             y = WRGirrigationTotal_mm_Reported)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = irrArea_goodFit), shape = 1, alpha = 0.5) +
  facet_wrap( ~ Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [mm]",
                     limits = c(0, max(df_wrg_irr_all$WRGirrigationTotal_mm_Reported, na.rm = T)),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, max(df_wrg_irr_all$WRGirrigationTotal_mm_Reported, na.rm = T)),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(name = "Area Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.red)) +  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrDepth_AllWRGs.png"),
       p_wrg_depth_all, width = 190, height = 110, units = "mm")


# Depth comparison - area agreement only ----------------------------------

p_wrg_depth_areaMatch <-
  ggplot(df_wrg_irr_areaMatch, aes(x = WRGirrigationTotal_mm_OpenET, 
                             y = WRGirrigationTotal_mm_Reported)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = factor(Year)), shape = 1) +
  facet_wrap( ~ Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [mm]",
                     limits = c(0, max(df_wrg_irr_areaMatch_simple$WRGirrigationTotal_mm_OpenET)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, max(df_wrg_irr_areaMatch_simple$WRGirrigationTotal_mm_OpenET)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrDepth_AllWRGs-AreaMatch.png"),
       p_wrg_depth_areaMatch, width = 190, height = 110, units = "mm")
