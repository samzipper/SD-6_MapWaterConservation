## RadarPrecip_Compare_OpenET-WIMAS_LEMA_allts.R
# This script is supposed to compare estimated pumping from WIMAS and OpenET at 
# scale of the LEMA for three timescales: Annual, Growing Season, Water Year.

source(file.path("code", "paths+packages.R"))

# load and join pumping estimates
df_wimas <- 
  read_csv(file.path("data", "WRGs_LEMAtotalIrrigation.csv")) |> 
  mutate(Algorithm = "Reported") |> 
  rename(Irrigation_m3 = WIMASirrigationLEMA_m3) |> 
  subset(Year >= 2016 & Year <= 2020)
df_yr <- 
  read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_Annual_RadarPrecip.csv")) |> 
  rename(Irrigation_m3 = OpenETirrigationLEMA_m3) |> 
  bind_rows(df_wimas) |> 
  mutate(ts = "Annual")
df_gs <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_GrowingSeason_RadarPrecip.csv")) |> 
  rename(Irrigation_m3 = OpenETirrigationLEMA_m3) |> 
  bind_rows(df_wimas) |> 
  mutate(ts = "Growing Season")

df_allts <- 
  bind_rows(df_yr, df_gs)

# set factor order for plotting
df_allts$Algorithm <- factor(df_allts$Algorithm, 
                             levels = c("Reported", "ensemble", "disalexi", 
                                        "eemetric", "geesebal", "ptjpl", "sims", "ssebop"))

# plot timeseries
ggplot(df_allts, aes(x = Year, y = Irrigation_m3/1e7, color = Algorithm)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(a) Annual (Jan-Dec)", 
                                      "Growing Season" = "(b) Growing Season (Apr-Oct)"))) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_y_continuous(name = "Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 3))
ggsave(file.path("figures+tables", "RadarPrecip_Compare_OpenET-WIMAS_LEMA_allts_Timeseries.png"),
       width = 95, height = 95, units = "mm")

# calculate fit statistics
getR2 <- function(y, x) summary(lm(y~x))$r.squared
getSlope <- function(y, x) coefficients(lm(y~x))[2]
df_fit_long <-
  df_allts |> 
  # have to pivot wider then longer to grab out the WIMAS data
  pivot_wider(id_cols = c("Year", "ts"), names_from = "Algorithm", values_from = "Irrigation_m3") |> 
  pivot_longer(cols = -c("Year", "ts", "Reported"), names_to = "Algorithm", values_to = "Irrigation_m3") |> 
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
  pivot_wider(id_cols = "Algorithm", names_from = c("metric", "ts"), values_from = "fit")

write_csv(df_fit_wide, file.path("figures+tables", "RadarPrecip_Compare_OpenET-WIMAS_LEMA_allts_FitStats.csv"))

# pull in annual precip to plot fit vs. precipitation
timestep <- "Annual"
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_met <- 
  read_csv(file.path("data", paste0("RadarPrecip_", timestep, "ByField.csv"))) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema) |> 
  mutate(precip_m3 = (precip_mm/1000)*area_m2) |> 
  group_by(Year) |> 
  summarize(TotalPrecip_m3 = sum(precip_m3),
            MeanPrecip_mm = mean(precip_mm))

df_fit_with_precip <-
  df_allts |> 
  # have to pivot wider then longer to grab out the WIMAS data
  pivot_wider(id_cols = c("Year", "ts"), names_from = "Algorithm", values_from = "Irrigation_m3") |> 
  pivot_longer(cols = -c("Year", "ts", "Reported"), names_to = "Algorithm", values_to = "Irrigation_m3") |> 
  subset(ts == timestep) |> 
  mutate(IrrDiff_m3 = Irrigation_m3 - Reported) |> 
  left_join(fields_met, by = "Year")

p_fit_precip <-
  ggplot(subset(df_fit_with_precip, Algorithm == "ensemble"), aes(x = MeanPrecip_mm, 
                                                                  y = IrrDiff_m3/1e7, 
                                                                  color = Algorithm)) +
  geom_hline(yintercept = 0, color = col.gray) +
  stat_smooth(method = "lm") +
  geom_point() +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms,
                     guide = NULL) +
  scale_x_continuous(name = "Mean Precipitation [mm]", expand = expansion(mult = 0.06)) +
  scale_y_continuous(name = "Estimated - Reported Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0))
ggsave(file.path("figures+tables", "RadarPrecip_Compare_OpenET-WIMAS_LEMA_FitVsPrecip.png"),
       p_fit_precip, width = 95, height = 95, units = "mm")

summary(lm(IrrDiff_m3/1e7 ~ MeanPrecip_mm, data = subset(df_fit_with_precip, Algorithm == "ensemble")))

# bar chart of some stats by timescale
ggplot(df_fit_long, aes(x = Algorithm, y = fit, fill = ts)) +
  geom_col(position = "dodge") +
  facet_wrap(~metric, nrow = 1, scales = "free_x",
             labeller = as_labeller(c("Bias_prc" = "Bias [%]",
                                      "MAE_1e7m3" = "MAE [x10\u2077 m\u00b3]",
                                      "R2" = "R\u00b2",
                                      "slope" = "Slope"))) +
  scale_fill_discrete(name = "Aggregation Timescale") +
  scale_x_discrete(labels = labs_algorithms) +
  scale_y_continuous(name = "Fit Statistic") +
  coord_flip() +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "RadarPrecip_Compare_OpenET-WIMAS_LEMA_allts_FitStats_BarChart.png"),
       width = 190, height = 95, units = "mm")
