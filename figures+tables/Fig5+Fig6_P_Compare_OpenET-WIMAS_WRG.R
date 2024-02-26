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

## plot - comparison of irrigated area
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
  scale_shape_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = 1, "TRUE" = 16)) +
  scale_color_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.org)) +
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(1,0))

ggsave(file.path("figures+tables", "Fig6_Compare_OpenET-WIMAS_WRG_IrrArea_AllWRGs.png"),
       p_irrArea_compare, width = 95, height = 95, units = "mm")

p_irrArea_compare_ac <-
  ggplot(wrg_use_plot, aes(x = irrFieldArea_m2/4046.86, y = WRGirrAreaReported_m2/4046.86)) +
  geom_point(aes(shape = irrArea_goodFit, color = irrArea_goodFit)) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.org) +
  scale_x_continuous(name = "Calculated Irrigated Area [acres]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use_plot$irrFieldArea_m2/4046.86, na.rm = T))) +
  scale_y_continuous(name = "Reported Irrigated Area [acres]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use_plot$irrFieldArea_m2/4046.86, na.rm = T))) +
  coord_equal() +
  scale_shape_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = 1, "TRUE" = 16)) +
  scale_color_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.org)) +
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(1,0))

ggsave(file.path("figures+tables", "Fig6_Compare_OpenET-WIMAS_WRG_IrrArea_AllWRGs_Acres.png"),
       p_irrArea_compare_ac, width = 95, height = 95, units = "mm")

############ FIGURE FOR PAPER

# algorithm for figure
alg_fig <- "ensemble"
lema_only <- T  # only plot LEMA fields or buffer too?

if (lema_only) {
  df_wrg_irr_plot <- subset(df_wrg_irr_all, LEMA_irrFieldArea_fraction > 0.5)
} else {
  df_wrg_irr_plot <- df_wrg_irr_all
}

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

# calculate multi-year averages
df_wrg_irr_plot_avg <-
  df_wrg_irr_plot |> 
  group_by(WR_GROUP, Algorithm) |> 
  summarize(WRGirrigationTotal_m3_OpenET_avg = mean(WRGirrigationTotal_m3_OpenET),
            WRGirrigationTotal_m3_Reported_avg = mean(WRGirrigationTotal_m3_Reported),
            WRGirrigationTotal_mm_OpenET_avg = mean(WRGirrigationTotal_mm_OpenET),
            WRGirrigationTotal_mm_Reported_avg = mean(WRGirrigationTotal_mm_Reported))

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
                  tag_suffix = ")")
ggsave(file.path("figures+tables", "Fig5_Compare_OpenET-WIMAS_WRGs.png"),
       width = 170, height = 130, units = "mm")

# acre-feet version


# panel (a): volume comparison, all WRGs
p_a_af <-
  ggplot(subset(df_wrg_irr_plot, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_m3_OpenET*0.000810714/1e3, 
             y = WRGirrigationTotal_m3_Reported*0.000810714/1e3,
             color = factor(Year))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Calculated Irrigation [x1000 ac-ft]",
                     limits = c(0, 1.7),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x1000 ac-ft]",
                     limits = c(0, 1.7),
                     expand = expansion(mult = c(0, 0.025))) +  
  scale_color_viridis_d(name = "Year") +
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (b): depth comparison, all WRGs
p_b_af <- 
  ggplot(subset(df_wrg_irr_plot, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_mm_OpenET/25.4, 
             y = WRGirrigationTotal_mm_Reported/25.4,
             color = factor(Year))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Calculated Irrigation [in]",
                     limits = c(0, 600/25.4),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Reported Irrigation [in]",
                     limits = c(0, 600/25.4),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_color_viridis_d(name = "Year") +
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (c): avg volume comparison, all WRGs
p_c_af <- 
  ggplot(subset(df_wrg_irr_plot_avg, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_m3_OpenET_avg*0.000810714/1e3, 
             y = WRGirrigationTotal_m3_Reported_avg*0.000810714/1e3)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = pal_algorithms[alg_fig]) +
  scale_x_continuous(name = "Avg. Est. Irrigation [x1000 ac-ft]",
                     limits = c(0, 0.8),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Avg. Rep. Irrigation [x1000 ac-ft]",
                     limits = c(0, 0.8),
                     expand = expansion(mult = c(0, 0.025))) +  
  #scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  NULL

# panel (d): average depth comparison, all WRGs
p_d_af <- 
  ggplot(subset(df_wrg_irr_plot_avg, Algorithm == alg_fig), 
         aes(x = WRGirrigationTotal_mm_OpenET_avg/25.4, 
             y = WRGirrigationTotal_mm_Reported_avg/25.4)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = pal_algorithms[alg_fig]) +
  scale_x_continuous(name = "Avg. Calculated Irrigation [in]",
                     limits = c(0, 375/25.4),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(name = "Avg. Reported Irrigation [in]",
                     limits = c(0, 375/25.4),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_equal() +
  NULL

(p_a_af + p_b_af + guide_area() + p_c_af + p_d_af + plot_spacer()) +
  plot_layout(ncol = 3, guides = "collect",
              widths = c(1, 1, 0.3)) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")
ggsave(file.path("figures+tables", "Fig5_Compare_OpenET-WIMAS_WRGs_AcreFeet.png"),
       width = 160, height = 130, units = "mm")

# calculate fit stats
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]
df_fit_plot <-
  df_wrg_irr_plot |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            slope = getSlope(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported))

#write_csv(df_fit_plot, file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_FitStats_AllWRGs-AreaMatch.csv"))

df_fit_plot_avg <-
  df_wrg_irr_plot_avg |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET_avg, WRGirrigationTotal_m3_Reported_avg),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET_avg, WRGirrigationTotal_m3_Reported_avg),
            slope = getSlope(WRGirrigationTotal_m3_OpenET_avg, WRGirrigationTotal_m3_Reported_avg))

df_fit_plot_avg_mm <-
  df_wrg_irr_plot_avg |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            MAE_1e5m3 = mae(WRGirrigationTotal_mm_OpenET_avg/1e5, WRGirrigationTotal_mm_Reported_avg/1e5),
            R2 = getR2(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            slope = getSlope(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg))

# check
summary(lm(WRGirrigationTotal_m3_Reported_avg ~ WRGirrigationTotal_m3_OpenET_avg, data = subset(df_wrg_irr_plot_avg, Algorithm == alg_fig)))
ggplot(subset(df_wrg_irr_plot_avg, Algorithm == alg_fig), aes(x = WRGirrigationTotal_m3_OpenET_avg, y = WRGirrigationTotal_m3_Reported_avg)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

### SI FIGURE: comparison for only fields with area agreement

# Volume comparison - area agreement only ---------------------------------

# subset to good area match only
df_wrg_irr_areaMatch <-
  df_wrg_irr_all |> 
  subset(irrArea_goodFit)


df_wrg_irr_areaMatch_avg <-
  df_wrg_irr_areaMatch |> 
  group_by(WR_GROUP, Algorithm) |> 
  summarize(WRGirrigationTotal_m3_OpenET_avg = mean(WRGirrigationTotal_m3_OpenET),
            WRGirrigationTotal_m3_Reported_avg = mean(WRGirrigationTotal_m3_Reported),
            WRGirrigationTotal_mm_OpenET_avg = mean(WRGirrigationTotal_mm_OpenET),
            WRGirrigationTotal_mm_Reported_avg = mean(WRGirrigationTotal_mm_Reported))

# panel (a): volume comparison, all WRGs
p_a_areaMatch <-
  ggplot(subset(df_wrg_irr_areaMatch, Algorithm == alg_fig), 
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
p_b_areaMatch <- 
  ggplot(subset(df_wrg_irr_areaMatch, Algorithm == alg_fig), 
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
p_c_areaMatch <- 
  ggplot(subset(df_wrg_irr_areaMatch_avg, Algorithm == alg_fig), 
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
p_d_areaMatch <- 
  ggplot(subset(df_wrg_irr_areaMatch_avg, Algorithm == alg_fig), 
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

(p_a_areaMatch + p_b_areaMatch + guide_area() + 
    p_c_areaMatch + p_d_areaMatch + plot_spacer()) +
  plot_layout(ncol = 3, guides = "collect",
              widths = c(1, 1, 0.25)) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")
ggsave(file.path("figures+tables", "FigS12_Compare_OpenET-WIMAS_WRGs-AreaMatch.png"),
       width = 170, height = 130, units = "mm")

# fit stats
df_fit_plot_areaMatch <-
  df_wrg_irr_areaMatch |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            slope = getSlope(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported))

df_fit_plot_areaMatch_avg <-
  df_wrg_irr_areaMatch_avg |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET_avg, WRGirrigationTotal_m3_Reported_avg),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET_avg/1e5, WRGirrigationTotal_m3_Reported_avg/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET_avg, WRGirrigationTotal_m3_Reported_avg),
            slope = getSlope(WRGirrigationTotal_m3_OpenET_avg, WRGirrigationTotal_m3_Reported_avg))

df_fit_plot_areaMatch_avg_mm <-
  df_wrg_irr_areaMatch_avg |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            MAE_1e5m3 = mae(WRGirrigationTotal_mm_OpenET_avg/1e5, WRGirrigationTotal_mm_Reported_avg/1e5),
            R2 = getR2(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg),
            slope = getSlope(WRGirrigationTotal_mm_OpenET_avg, WRGirrigationTotal_mm_Reported_avg))

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
  scale_x_continuous(name = "Calculated Irrigation [x10\u2075 m\u00b3]",
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
  scale_x_continuous(name = "Calculated Irrigation [x10\u2075 m\u00b3]",
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
  scale_x_continuous(name = "Calculated Irrigation [mm]",
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
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, max(df_wrg_irr_areaMatch$WRGirrigationTotal_mm_OpenET)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, max(df_wrg_irr_areaMatch$WRGirrigationTotal_mm_OpenET)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrDepth_AllWRGs-AreaMatch.png"),
       p_wrg_depth_areaMatch, width = 190, height = 110, units = "mm")