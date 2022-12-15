# Compare_OpenET-WIMAS_WRG_IrrigatedArea.R

source(file.path("code", "paths+packages.R"))

# timescale for OpenET data
ts <- "GrowingSeason"

# load WRG data
wrg_use <- 
  read_csv(file.path("data", "WRGs_UseByWRG.csv")) |> 
  mutate(irrFieldArea_m2 = LEMA_irrFieldArea_m2 + notLEMA_irrFieldArea_m2) |> 
  subset(Year <= 2020) |> 
  subset(is.finite(WRGirrAreaReported_m2)) # lots of missing values

# set threshold of irrigated area agreement for "good fits"
prc_thres <- 0.1  # 10%

# identify good fits
wrg_use$irrArea_pctDiff <- (wrg_use$irrFieldArea_m2 - wrg_use$WRGirrAreaReported_m2)/wrg_use$WRGirrAreaReported_m2
wrg_use$irrArea_goodFit <- abs(wrg_use$irrArea_pctDiff) <= prc_thres

## plot - comparison of irrigated area
p_irrArea_compare <-
  ggplot(wrg_use, aes(x = irrFieldArea_m2/1e4, y = WRGirrAreaReported_m2/1e4)) +
  geom_point(aes(shape = irrArea_goodFit, color = irrArea_goodFit)) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  scale_x_continuous(name = "Irrigated Area, Sum of Fields [m\u00b2]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use$irrFieldArea_m2/1e4, na.rm = T))) +
  scale_y_continuous(name = "Irrigated Area, Reported [m\u00b2]",
                     expand = expansion(mult = c(0, 0.025)),
                     limits = c(0, max(wrg_use$irrFieldArea_m2/1e4, na.rm = T))) +
  coord_equal() +
  scale_shape_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = 1, "TRUE" = 16)) +
  scale_color_manual(name = "Agreement", labels = c("FALSE" = "> 10%", "TRUE" = "< 10%"), 
                     values = c("FALSE" = col.gray, "TRUE" = col.cat.red)) +
  theme(legend.position = c(0.99,0.01), 
        legend.justification = c(1,0))

ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrArea.png"),
       p_irrArea_compare, width = 95, height = 95, units = "mm")

## subset to only good fits and compare OpenET irrigation and flowmeter irrigation
# subset use to only good WRGs
wrg_use_good <- subset(wrg_use, irrArea_goodFit)

# load field for each WRG and irrigation for each field
wrg_fields <- 
  read_csv(file.path("data", "WRGs_WRGbyField.csv")) |> 
  subset(WR_GROUP %in% wrg_use_good$WR_GROUP)

df_irr <- 
  read_csv(file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv"))) |> 
  subset(UID %in% wrg_fields$UID)

df_wrg_irr <-
  df_irr |> 
  left_join(wrg_fields, by = "UID") |> 
  group_by(WR_GROUP, Year, Algorithm) |> 
  summarize(WRGirrigationTotal_m3 = sum(FieldIrrigation_m3)) |> 
  left_join(wrg_use_good, by = c("Year", "WR_GROUP"), suffix = c("_OpenET", "_Reported")) |> 
  subset(is.finite(WRGirrigationTotal_m3_Reported))

# plot
p_wrg_fit <-
  ggplot(df_wrg_irr, aes(x = WRGirrigationTotal_m3_OpenET/1e5, 
                       y = WRGirrigationTotal_m3_Reported/1e5)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = factor(Year)), shape = 1) +
  facet_wrap( ~ Algorithm, nrow = 2, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(name = "Reported Irrigation [x10\u2075 m\u00b3]",
                     limits = c(0, max(df_wrg_irr$WRGirrigationTotal_m3_OpenET/1e5)),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Year", values = c(col.cat.blu, col.cat.grn, col.cat.yel, col.cat.org, col.cat.red)) +
  coord_equal() +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL
ggsave(file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_IrrVolume.png"),
       p_wrg_fit, width = 190, height = 110, units = "mm")

## calculate fit stats
getR2 <- function(y, x) summary(lm(y~x))$r.squared
getSlope <- function(y, x) coefficients(lm(y~x))[2]
df_fit_long <-
  df_wrg_irr |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            MAE_1e5m3 = mae(WRGirrigationTotal_m3_OpenET/1e5, WRGirrigationTotal_m3_Reported/1e5),
            R2 = getR2(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported),
            slope = getSlope(WRGirrigationTotal_m3_OpenET, WRGirrigationTotal_m3_Reported))

write_csv(df_fit_long, file.path("figures+tables", "Compare_OpenET-WIMAS_WRG_FitStats.csv"))
