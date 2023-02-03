## RadarPrecip_CompareLEMAirrigationToGridmet.R

source(file.path("code", "paths+packages.R"))

# load data
df_yr_radar <- 
  read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_GrowingSeason_RadarPrecip.csv")) |> 
  mutate(Source = "Radar Precip")
df_yr_gridmet <- 
  read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_GrowingSeason.csv")) |> 
  mutate(Source = "gridMET Precip")

df_wimas <- 
  read_csv(file.path("data", "WRGs_LEMAtotalIrrigation.csv")) |> 
  mutate(Algorithm = "Reported") |> 
  rename(Irrigation_m3 = WIMASirrigationLEMA_m3) |> 
  subset(Year >= 2016 & Year <= 2020)

# join and plot
df_both <- 
  left_join(df_yr_radar, df_yr_gridmet, by = c("Algorithm", "Year"), suffix = c("_radar", "_gridmet"))

ggplot(df_both, aes(x = OpenETirrigationLEMA_m3_gridmet/1e7, y = OpenETirrigationLEMA_m3_radar/1e7)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = Algorithm)) +
  facet_wrap(~Year) +
  scale_x_continuous(name = "LEMA Irrigation from gridMET Precipitation [x10\u2077 m\u00b3]") +
  scale_y_continuous(name = "LEMA Irrigation from Radar Precipitation [x10\u2077 m\u00b3]") +
  scale_color_manual(name = "Algorithm", values = pal_algorithms, labels = labs_algorithms,
                     guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "RadarPrecip_CompareLEMAirrigationToGridmet_AnnualScatter.png"),
       width = 190, height = 120, units = "mm")

# plot both against WIMAS
df_both_with_wimas <-
  bind_rows(df_yr_radar, df_yr_gridmet) |> 
  left_join(df_wimas[,c("Year", "Irrigation_m3")], by = "Year")

ggplot(df_both_with_wimas, aes(x = Irrigation_m3/1e7, y = OpenETirrigationLEMA_m3/1e7, 
                               color = Algorithm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  facet_wrap(~Source, ncol = 1) +
  scale_x_continuous(name = "Reported LEMA Irrigation [x10\u2077 m\u00b3]", 
                     limits = c(min(df_both_with_wimas$OpenETirrigationLEMA_m3), 
                                max(df_both_with_wimas$OpenETirrigationLEMA_m3))/1e7) +
  scale_y_continuous(name = "Estimated LEMA Irrigation from OpenET [x10\u2077 m\u00b3]") +
  scale_color_manual(name = "Algorithm", values = pal_algorithms, labels = labs_algorithms) +
  coord_equal()

ggsave(file.path("figures+tables", "RadarPrecip_CompareLEMAirrigationToGridmet_BySource.png"),
       width = 120, height = 190, units = "mm")

# calculate fit
df_fit <-
  df_both_with_wimas |> 
  group_by(Algorithm, Source) |> 
  summarize(MAE = mae(OpenETirrigationLEMA_m3, 
                      Irrigation_m3),
            RMSE = rmse(OpenETirrigationLEMA_m3, 
                      Irrigation_m3))

ggplot(df_fit, aes(x = Algorithm, y = RMSE/1e7, fill = Source)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = labs_algorithms) +
  scale_y_continuous(name = "Root Mean Squared Difference [x10\u2077 m\u00b3]\nbetween Estimated and Reported Irrigation",
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Precipitation Source", labels = c("gridMET", "Radar"),
                    values = c("#99d8c9", "#2ca25f")) +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "RadarPrecip_CompareLEMAirrigationToGridmet_ErrorBarChart.png"),
       width = 190, height = 95, units = "mm")
