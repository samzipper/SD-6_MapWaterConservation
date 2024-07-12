## Compare_OpenET-WIMAS_LEMA_allts.R
# This script is supposed to compare estimated pumping from WIMAS and OpenET at 
# scale of the LEMA.

source(file.path("code", "paths+packages.R"))

# load and join pumping estimates
df_wimas <- 
  read_csv(file.path("data", "WRGs_LEMAtotalIrrigation.csv")) |> 
  mutate(Algorithm = "Reported") |> 
  rename(Irrigation_m3 = WIMASirrigationLEMA_m3) |> 
  subset(Year >= 2016 & Year <= 2020)

# choose LEMA total estimates - all irrigated fields, or high confidence only
irr_choice <- "All" # "All" or "HighConf"
if (irr_choice == "HighConf"){
  df_yr <- 
    read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_Annual_HighConfOnly.csv")) |> 
    rename(Irrigation_m3 = OpenETirrigationLEMAPeff_m3) |> 
    bind_rows(df_wimas) |> 
    mutate(ts = "Annual")
  df_wyr <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_WaterYear_HighConfOnly.csv")) |> 
    rename(Irrigation_m3 = OpenETirrigationLEMAPeff_m3) |> 
    bind_rows(df_wimas) |> 
    mutate(ts = "Water Year")
  df_gs <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_GrowingSeason_HighConfOnly.csv")) |> 
    rename(Irrigation_m3 = OpenETirrigationLEMAPeff_m3) |> 
    bind_rows(df_wimas) |> 
    mutate(ts = "Growing Season")
} else if (irr_choice == "All"){
  df_yr <- 
    read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_Annual.csv")) |> 
    rename(Irrigation_m3 = OpenETirrigationLEMAPeff_m3) |> 
    bind_rows(df_wimas) |> 
    mutate(ts = "Annual")
  df_wyr <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_WaterYear.csv")) |> 
    rename(Irrigation_m3 = OpenETirrigationLEMAPeff_m3) |> 
    bind_rows(df_wimas) |> 
    mutate(ts = "Water Year")
  df_gs <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_GrowingSeason.csv")) |> 
    rename(Irrigation_m3 = OpenETirrigationLEMAPeff_m3) |> 
    bind_rows(df_wimas) |> 
    mutate(ts = "Growing Season")
}


# what timestep to plot?
ts_plot <- "GrowingSeason"

df_plot <- df_gs

# set factor order for plotting
df_plot$Algorithm <- factor(df_plot$Algorithm, 
                             levels = c("Reported", "ensemble", "disalexi", 
                                        "eemetric", "geesebal", "ptjpl", "sims", "ssebop"))

# have to pivot wider then longer to grab out the WIMAS data
df_plot_wide <- 
  df_plot |> 
  pivot_wider(id_cols = c("Year", "ts"), names_from = "Algorithm", values_from = "Irrigation_m3") |> 
  pivot_longer(cols = -c("Year", "ts", "Reported"), names_to = "Algorithm", values_to = "Irrigation_m3")
  
# calculate multi-year average
df_plot_avg <-
  df_plot |> 
  group_by(Algorithm, ts) |> 
  summarize(Irrigation_m3_mean = mean(Irrigation_m3),
            Irrigation_m3_std = sd(Irrigation_m3),
            Irrigation_m3_min = min(Irrigation_m3),
            Irrigation_m3_max = max(Irrigation_m3))

## FIGURE 8 - timeseries and precip error
# plot timeseries
p_timeseries <- 
  ggplot(df_plot, aes(x = Year, y = Irrigation_m3/1e7, color = Algorithm)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_y_continuous(name = "Annual Irrigation [x10\u2077 m\u00b3]",
                     limits = c(0, 6.5)) +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

# calculate fit statistics
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]
df_fit_long <-
  df_plot_wide |> 
  # calculate stats for ts, algorithm
  group_by(ts, Algorithm) |> 
  summarize(Bias_prc = pbias(Irrigation_m3, Reported),
            MAE_1e7m3 = mae(Irrigation_m3/1e7, Reported/1e7),
            R2 = getR2(Irrigation_m3, Reported),
            slope = getSlope(Irrigation_m3, Reported)) |> 
  pivot_longer(cols = c("Bias_prc", "MAE_1e7m3", "R2", "slope"), 
               names_to = "metric", values_to = "fit")

df_fit_wide <-
  df_fit_long |> 
  # pivot longer then wider for paper table formatting
  arrange(metric, ts, Algorithm) |> 
  pivot_wider(id_cols = "Algorithm", names_from = "metric", values_from = "fit")

# pull in annual precip to plot fit vs. precipitation
timestep <- "GrowingSeason"
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_met <- 
  read_csv(file.path("data", paste0("gridmet_", gsub(" ", "", timestep), "ByField.csv"))) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema) |> 
  mutate(precip_m3 = (precip_mm/1000)*area_m2) |> 
  #rename(Year = WaterYear) |> # needed if you choose WaterYear timestep
  group_by(Year) |> 
  summarize(TotalPrecip_m3 = sum(precip_m3),
            MeanPrecip_mm = mean(precip_mm))

df_fit_with_precip <-
  df_plot_wide |> 
  mutate(IrrDiff_m3 = Irrigation_m3 - Reported) |> 
  left_join(fields_met, by = "Year")

p_fit_precip <-
  ggplot(subset(df_fit_with_precip, Algorithm == "ensemble"), aes(x = MeanPrecip_mm, 
                                                                  y = IrrDiff_m3/1e7, 
                                                                  color = Algorithm)) +
  geom_hline(yintercept = 0, color = col.gray) +
  stat_smooth(method = "lm") +
  geom_point() +
  geom_text(aes(label = Year), check_overlap = T, 
            hjust = c(1, 1, 0, 0.5, 0), 
            vjust = c(0.5, 0.5, 0.5, 0, 0.5), 
            nudge_x = c(-10, -10, 10, 8, 10),
            nudge_y = c(0, 0, 0, 0.10, 0.05)) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms,
                     guide = NULL) +
  scale_x_continuous(name = "Growing Season Precipitation [mm]",
                     expand = expansion(mult = c(0.025, 0.085))) +
  scale_y_continuous(name = "(Calculated - Reported)\nIrrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0))
#ggsave(file.path("figures+tables", "Fig4_Peff_Compare_OpenET-WIMAS_LEMA_FitVsPrecip.png"),
#       p_fit_precip, width = 95, height = 95, units = "mm")

summary(lm(IrrDiff_m3/1e7 ~ MeanPrecip_mm, data = subset(df_fit_with_precip, Algorithm == "ensemble")))

# combine and save
(p_timeseries + p_fit_precip) +
  plot_layout(ncol = 2, widths = c(1.5, 1)) +
  plot_annotation(tag_levels = "a", tag_prefix = " (", tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag.position = "topleft",
        plot.tag.location = "panel")
ggsave(file.path("figures+tables", "Fig8_LEMA-Timeseries+PrecipScatter.png"),
       width = 190, height = 95, units = "mm")

## FIGURE 9 - CORRECTED TIMESERIES
# Area correction - remove for now
# # shift based on irrigated area
# prc_shift <- (1-0.069) # amount to reduce, based on WRG irrigated area comparison
# df_plot$Irrigation_m3_shift <- df_plot$Irrigation_m3
# df_plot$Irrigation_m3_shift[df_plot$Algorithm != "Reported"] <- 
#   df_plot$Irrigation_m3[df_plot$Algorithm != "Reported"]*prc_shift
# df_plot_wide$Irrigation_m3_shift <- df_plot_wide$Irrigation_m3*prc_shift
# 
# # fit stats
# df_fit_long_shift <-
#   df_plot_wide |> 
#   # calculate stats for ts, algorithm
#   group_by(ts, Algorithm) |> 
#   summarize(Bias_prc = pbias(Irrigation_m3_shift, Reported),
#             MAE_1e7m3 = mae(Irrigation_m3_shift/1e7, Reported/1e7),
#             R2 = getR2(Irrigation_m3_shift, Reported),
#             slope = getSlope(Irrigation_m3_shift, Reported)) |> 
#   pivot_longer(cols = c("Bias_prc", "MAE_1e7m3", "R2", "slope"), 
#                names_to = "metric", values_to = "fit")
# 
# df_fit_wide_shift <-
#   df_fit_long_shift |> 
#   # pivot longer then wider for paper table formatting
#   arrange(metric, ts, Algorithm) |> 
#   pivot_wider(id_cols = "Algorithm", names_from = "metric", values_from = "fit")
# 
# # plot - area-adjusted
# p_timeseries_areaShift <- 
#   ggplot(df_plot, aes(x = Year, y = Irrigation_m3_shift/1e7, color = Algorithm)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
#   scale_y_continuous(name = "Area-Adjusted\nAnnual Irrigation [x10\u2077 m\u00b3]",
#                      limits = c(0, 6.5)) +
#   theme(legend.position = "bottom",
#         strip.text = element_text(hjust = 0)) +
#   guides(color = guide_legend(nrow = 3))
#
# statistical bias-correction of calculated irrigation using precip
# pull out each algorithm
df_ensemble <- subset(df_fit_with_precip, Algorithm == "ensemble")
df_disalexi <- subset(df_fit_with_precip, Algorithm == "disalexi")
df_eemetric <- subset(df_fit_with_precip, Algorithm == "eemetric")
df_geesebal <- subset(df_fit_with_precip, Algorithm == "geesebal")
df_ptjpl <- subset(df_fit_with_precip, Algorithm == "ptjpl")
df_sims <- subset(df_fit_with_precip, Algorithm == "sims")
df_ssebop <- subset(df_fit_with_precip, Algorithm == "ssebop")

# linear fit to precip
lm_ensemble <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_ensemble)
lm_disalexi <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_disalexi)
lm_eemetric <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_eemetric)
lm_geesebal <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_geesebal)
lm_ptjpl <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_ptjpl)
lm_sims <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_sims)
lm_ssebop <- lm(IrrDiff_m3 ~ MeanPrecip_mm, data = df_ssebop)

# bias correct
df_ensemble$Irrigation_m3_precipCorrect <- df_ensemble$Irrigation_m3 - predict(lm_ensemble, data = df_ensemble)
df_disalexi$Irrigation_m3_precipCorrect <- df_disalexi$Irrigation_m3 - predict(lm_disalexi, data = df_ensemble)
df_eemetric$Irrigation_m3_precipCorrect <- df_eemetric$Irrigation_m3 - predict(lm_eemetric, data = df_eemetric)
df_geesebal$Irrigation_m3_precipCorrect <- df_geesebal$Irrigation_m3 - predict(lm_geesebal, data = df_geesebal)
df_ptjpl$Irrigation_m3_precipCorrect <- df_ptjpl$Irrigation_m3 - predict(lm_ptjpl, data = df_ptjpl)
df_sims$Irrigation_m3_precipCorrect <- df_sims$Irrigation_m3 - predict(lm_sims, data = df_sims)
df_ssebop$Irrigation_m3_precipCorrect <- df_ssebop$Irrigation_m3 - predict(lm_ssebop, data = df_ssebop)

# combine back into one
df_wimas$Irrigation_m3_precipCorrect <- df_wimas$Irrigation_m3 # need to create this column for plotting (even though no correction applied to reported data)
df_precipCorrect <-
  bind_rows(df_ensemble, df_disalexi, df_eemetric, df_geesebal, df_ptjpl, df_sims, df_ssebop, df_wimas)

# set factor order for plotting
df_precipCorrect$Algorithm <- factor(df_precipCorrect$Algorithm, 
                                     levels = c("Reported", "ensemble", "disalexi", 
                                                "eemetric", "geesebal", "ptjpl", "sims", "ssebop"))

# plot - precip-adjusted
p_timeseries_precipCorrect <- 
  ggplot(df_precipCorrect, aes(x = Year, y = Irrigation_m3_precipCorrect/1e7, color = Algorithm)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_y_continuous(name = "Precip-Adjusted\nAnnual Irrigation [x10\u2077 m\u00b3]", 
                     limits = c(0,6.5)) +
  theme(strip.text = element_text(hjust = 0))

ggsave(file.path("figures+tables", "Fig9_LEMA-TimeseriesCorrected.png"),
       p_timeseries_precipCorrect, width = 190, height = 80, units = "mm")

### TABLE - fit stats for original, precip correct, area shift
df_fit_wide$Approach <- "Original"
#df_fit_wide_shift$Approach <- "AreaShift"

df_fit_precipCorrect <-
  df_precipCorrect |> 
  subset(Algorithm != "Reported") |> 
  # have to pivot wider then longer to grab out the WIMAS data
  # calculate stats for ts, algorithm
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(Irrigation_m3_precipCorrect, Reported),
            MAE_1e7m3 = mae(Irrigation_m3_precipCorrect/1e7, Reported/1e7),
            R2 = getR2(Irrigation_m3_precipCorrect, Reported),
            slope = getSlope(Irrigation_m3_precipCorrect, Reported)) |> 
  mutate(Approach = "PrecipCorrect")

df_fit_all <-
  #bind_rows(df_fit_wide, df_fit_precipCorrect, df_fit_wide_shift) |> 
  bind_rows(df_fit_wide, df_fit_precipCorrect) |> 
  pivot_longer(-c("Algorithm", "Approach")) |> 
  pivot_wider(id_cols = "Algorithm", names_from = c("name", "Approach"), values_from = "value") |> 
  dplyr::select(Algorithm, starts_with("Bias_"), starts_with("MAE_"), starts_with("R2_"), starts_with("slope_"))

write_csv(df_fit_all, file.path("figures+tables", "Table3_LEMA_FitStats.csv"))
