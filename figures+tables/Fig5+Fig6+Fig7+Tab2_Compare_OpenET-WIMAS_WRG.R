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
  summarize(WRGirrigationTotal_m3 = sum(FieldIrrigationPeff_m3)) |> 
  left_join(wrg_use_trim, by = c("Year", "WR_GROUP"), suffix = c("_OpenET", "_Reported")) |> 
  subset(is.finite(WRGirrigationTotal_m3_Reported))

# calculate irrigation depths
df_wrg_irr_all$WRGirrigationTotal_mm_Reported <- 1000*df_wrg_irr_all$WRGirrigationTotal_m3_Reported/df_wrg_irr_all$WRGirrAreaReported_m2
df_wrg_irr_all$WRGirrigationTotal_mm_OpenET <- 1000*df_wrg_irr_all$WRGirrigationTotal_m3_OpenET/df_wrg_irr_all$irrFieldArea_m2

# Irrigated area comparison -----------------------------------------------

lema_only <- T  # only plot LEMA fields or buffer too?

if (lema_only) {
  wrg_use_plot <- subset(wrg_use_trim, LEMA_irrFieldArea_fraction > 0.5)
} else {
  wrg_use_plot <- wrg_use_trim
}

# stats on agreement between Calculated and reported irrigated area
n_good <- sum(wrg_use_plot$irrArea_goodFit)
n_bad <- sum(!wrg_use_plot$irrArea_goodFit)
n_total <- length(wrg_use_plot$irrArea_goodFit)
print(paste0(n_good, " of ", n_total, " good (", 100*round(n_good/n_total, 2), "%)"))
print(paste0(n_bad, " of ", n_total, " bad (", 100*round(n_bad/n_total, 2), "%)"))

hist(wrg_use_plot$irrArea_pctDiff)
mean(wrg_use_plot$irrArea_pctDiff[wrg_use_plot$WRGirrAreaReported_m2 > 0])
median(wrg_use_plot$irrArea_pctDiff)

## Figure - irrigated area comparison
meanIrrAreaDiff <- round(mean(wrg_use_plot$irrArea_pctDiff[wrg_use_plot$WRGirrAreaReported_m2 > 0]), 3)

p_irrArea_compare <-
  ggplot(wrg_use_plot, aes(x = irrFieldArea_m2/1e4, y = WRGirrAreaReported_m2/1e4)) +
  geom_point(aes(shape = irrArea_goodFit, color = irrArea_goodFit)) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.org) +
  scale_x_continuous(name = "Calculated Irrigated Area [ha]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use_plot$irrFieldArea_m2/1e4, na.rm = T))) +
  scale_y_continuous(name = "Reported Irrigated Area [ha]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use_plot$irrFieldArea_m2/1e4, na.rm = T))) +
  coord_equal() +
  annotate("text", x = 130, y = 450, 
           label = paste0("Mean Difference = ", meanIrrAreaDiff*100, "%")) +
  scale_shape_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = 1, "TRUE" = 16)) +
  scale_color_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.org)) +
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(1,0))

ggsave(file.path("figures+tables", "Fig6_Compare_OpenET-WIMAS_WRG_IrrArea_AllWRGs.png"),
       p_irrArea_compare, width = 95, height = 95, units = "mm")


## Figure- irrigation comparison, all LEMA WRGs

# algorithm for figure
alg_fig <- "ensemble"
lema_only <- T  # only plot LEMA fields or buffer too?

if (lema_only) {
  df_wrg_irr_plot <- subset(df_wrg_irr_all, LEMA_irrFieldArea_fraction > 0.5)
} else {
  df_wrg_irr_plot <- df_wrg_irr_all
}

# calculate multi-year averages
df_wrg_irr_plot_avg <-
  df_wrg_irr_plot |> 
  group_by(WR_GROUP, Algorithm) |> 
  summarize(WRGirrigationTotal_m3_OpenET_avg = mean(WRGirrigationTotal_m3_OpenET),
            WRGirrigationTotal_m3_Reported_avg = mean(WRGirrigationTotal_m3_Reported),
            WRGirrigationTotal_mm_OpenET_avg = mean(WRGirrigationTotal_mm_OpenET),
            WRGirrigationTotal_mm_Reported_avg = mean(WRGirrigationTotal_mm_Reported))

# panel (a): volume comparison, all WRGs
p_a <-
  ggplot(subset(df_wrg_irr_plot, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_m3_OpenET/1e5, 
             y = WRGirrigationTotal_m3_Reported/1e5,
             color = factor(Year))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Calculated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +  
  scale_color_viridis_d(name = "Year") +
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (b): depth comparison, all WRGs
p_b <- 
  ggplot(subset(df_wrg_irr_plot, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_mm_OpenET, 
             y = WRGirrigationTotal_mm_Reported,
             color = factor(Year))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, 600),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, 600),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_color_viridis_d(name = "Year") +
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (c): avg volume comparison, all WRGs
p_c <- 
  ggplot(subset(df_wrg_irr_plot_avg, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_m3_OpenET_avg/1e5, 
             y = WRGirrigationTotal_m3_Reported_avg/1e5)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = pal_algorithms[alg_fig]) +
  scale_x_continuous(name = "Avg. Calculated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Avg. Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +  
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (d): average depth comparison, all WRGs
p_d <- 
  ggplot(subset(df_wrg_irr_plot_avg, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_mm_OpenET_avg, 
             y = WRGirrigationTotal_mm_Reported_avg)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = pal_algorithms[alg_fig]) +
  scale_x_continuous(name = "Avg. Calculated Irrigation [mm]",
                     limits = c(0, 375),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Avg. Reported Irrigation [mm]",
                     limits = c(0, 375),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_equal() +
  NULL

(p_a + p_b + guide_area() + p_c + p_d + plot_spacer()) +
  plot_layout(ncol = 3, guides = "collect",
              widths = c(1, 1, 0.25)) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag.position = c(0.27, 0.95))
ggsave(file.path("figures+tables", "Fig5_Peff_Compare_OpenET-WIMAS_WRGs.png"),
       width = 170, height = 130, units = "mm")

lm(WRGirrigationTotal_mm_Reported ~ WRGirrigationTotal_mm_OpenET,
   data = subset(df_wrg_irr_plot, Algorithm == alg_fig)) |> summary()

lm(WRGirrigationTotal_mm_Reported_avg ~ WRGirrigationTotal_mm_OpenET_avg,
   data = subset(df_wrg_irr_plot_avg, Algorithm == alg_fig)) |> summary()

## Figure - area match comparison

# algorithm for figure
alg_fig <- "ensemble"
lema_only <- T  # only plot LEMA fields or buffer too?

if (lema_only) {
  df_wrg_irr_areaMatch <-
    df_wrg_irr_all |> 
    subset(irrArea_goodFit & 
             Algorithm == alg_fig &
             LEMA_irrFieldArea_fraction > 0.5)
} else {
  df_wrg_irr_areaMatch <-
    df_wrg_irr_all |> 
    subset(irrArea_goodFit & 
             Algorithm == alg_fig)
}

df_wrg_irr_areaMatch_avg <-
  df_wrg_irr_areaMatch |> 
  group_by(WR_GROUP, Algorithm) |> 
  summarize(WRGirrigationTotal_m3_OpenET_avg = mean(WRGirrigationTotal_m3_OpenET),
            WRGirrigationTotal_m3_Reported_avg = mean(WRGirrigationTotal_m3_Reported),
            WRGirrigationTotal_mm_OpenET_avg = mean(WRGirrigationTotal_mm_OpenET),
            WRGirrigationTotal_mm_Reported_avg = mean(WRGirrigationTotal_mm_Reported))

# panel (a): volume comparison, all WRGs
p_a_areaMatch <-
  ggplot(df_wrg_irr_areaMatch, 
         aes(x = WRGirrigationTotal_m3_OpenET/1e5, 
             y = WRGirrigationTotal_m3_Reported/1e5)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year)), shape = 1) +
  #stat_smooth(method = "lm", color = col.cat.red) +
  scale_x_continuous(name = "Calculated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +  
  scale_color_viridis_d(name = "Year") +
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (b): depth comparison, all WRGs
p_b_areaMatch <- 
  ggplot(df_wrg_irr_areaMatch, 
         aes(x = WRGirrigationTotal_mm_OpenET, 
             y = WRGirrigationTotal_mm_Reported)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Year)), shape = 1) +
  #stat_smooth(method = "lm", color = col.cat.red) +
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, 600),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, 600),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_color_viridis_d(name = "Year") +
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (c): avg volume comparison, all WRGs
p_c_areaMatch <- 
  ggplot(df_wrg_irr_areaMatch_avg, 
         aes(x = WRGirrigationTotal_m3_OpenET_avg/1e5, 
             y = WRGirrigationTotal_m3_Reported_avg/1e5)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = pal_algorithms[alg_fig]) +
  #stat_smooth(method = "lm", color = col.cat.red) +
  scale_x_continuous(name = "Avg. Calculated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Avg. Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, 20.5),
                     expand = expansion(mult = c(0, 0.025))) +  
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (d): average depth comparison, all WRGs
p_d_areaMatch <- 
  ggplot(df_wrg_irr_areaMatch_avg, 
         aes(x = WRGirrigationTotal_mm_OpenET_avg, 
             y = WRGirrigationTotal_mm_Reported_avg)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = pal_algorithms[alg_fig]) +
  #stat_smooth(method = "lm", color = col.cat.red) +
  scale_x_continuous(name = "Avg. Calculated Irrigation [mm]",
                     limits = c(0, 620),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Avg. Reported Irrigation [mm]",
                     limits = c(0, 620),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_equal() +
  NULL

(p_a_areaMatch + p_b_areaMatch + guide_area() + 
    p_c_areaMatch + p_d_areaMatch + plot_spacer()) +
  plot_layout(ncol = 3, guides = "collect",
              widths = c(1, 1, 0.25)) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag.position = c(0.27, 0.95))
ggsave(file.path("figures+tables", "Fig7_Peff_Compare_OpenET-WIMAS_WRGs-AreaMatch.png"),
       width = 170, height = 130, units = "mm")


lm(WRGirrigationTotal_mm_Reported ~ WRGirrigationTotal_mm_OpenET,
   data = subset(df_wrg_irr_areaMatch, Algorithm == alg_fig)) |> summary()

lm(WRGirrigationTotal_mm_Reported_avg ~ WRGirrigationTotal_mm_OpenET_avg,
   data = subset(df_wrg_irr_areaMatch_avg, Algorithm == alg_fig)) |> summary()

# fit statistics - table
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]
df_fit_sd6 <-
  df_wrg_irr_plot |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc_m3 = pbias(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            MAE_m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2_m3 = getR2(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            slope_m3 = getSlope(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            Bias_prc_mm = pbias(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported),
            MAE_mm = mae(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported),
            R2_mm = getR2(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported),
            slope_mm = getSlope(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported)) |> 
  mutate(time = "Annual",
         group = "SD6")

df_fit_sd6_avg <-
  df_wrg_irr_plot_avg |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc_m3 = pbias(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            MAE_m3 = mae(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            R2_m3 = getR2(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            slope_m3 = getSlope(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            Bias_prc_mm = pbias(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            MAE_mm = mae(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            R2_mm = getR2(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            slope_mm = getSlope(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg)) |> 
  mutate(time = "Avg",
         group = "SD6")

df_fit_areaMatch <-
  df_wrg_irr_areaMatch |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc_m3 = pbias(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            MAE_m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2_m3 = getR2(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            slope_m3 = getSlope(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            Bias_prc_mm = pbias(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported),
            MAE_mm = mae(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported),
            R2_mm = getR2(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported),
            slope_mm = getSlope(WRGirrigationTotal_mm_OpenET, WRGirrigationTotal_mm_Reported)) |> 
  mutate(time = "Annual",
         group = "areaMatch")

df_fit_areaMatch_avg <-
  df_wrg_irr_areaMatch_avg |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc_m3 = pbias(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            MAE_m3 = mae(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            R2_m3 = getR2(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            slope_m3 = getSlope(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            Bias_prc_mm = pbias(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            MAE_mm = mae(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            R2_mm = getR2(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            slope_mm = getSlope(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg)) |> 
  mutate(time = "Avg",
         group = "areaMatch")

df_fit_all <-
  bind_rows(df_fit_sd6, df_fit_sd6_avg, df_fit_areaMatch, df_fit_areaMatch_avg) |> 
  subset(Algorithm == alg_fig) |> 
  dplyr::select(-Algorithm)
write_csv(df_fit_all, file.path("figures+tables", "Table2_Peff_WRGFitStats.csv"))
