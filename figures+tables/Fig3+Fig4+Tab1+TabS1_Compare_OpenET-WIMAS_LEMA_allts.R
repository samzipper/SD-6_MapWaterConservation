## Compare_OpenET-WIMAS_LEMA_allts.R
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
  read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_Annual.csv")) |> 
  rename(Irrigation_m3 = OpenETirrigationLEMA_m3) |> 
  bind_rows(df_wimas) |> 
  mutate(ts = "Annual")
df_wyr <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_WaterYear.csv")) |> 
  rename(Irrigation_m3 = OpenETirrigationLEMA_m3) |> 
  bind_rows(df_wimas) |> 
  mutate(ts = "Water Year")
df_gs <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_GrowingSeason.csv")) |> 
  rename(Irrigation_m3 = OpenETirrigationLEMA_m3) |> 
  bind_rows(df_wimas) |> 
  mutate(ts = "Growing Season")

df_allts <- 
  bind_rows(df_yr, df_wyr, df_gs)

# set factor order for plotting
df_allts$Algorithm <- factor(df_allts$Algorithm, 
                             levels = c("Reported", "ensemble", "disalexi", 
                                        "eemetric", "geesebal", "ptjpl", "sims", "ssebop"))

# calculate multi-year average
df_allts_avg <-
  df_allts |> 
  group_by(Algorithm, ts) |> 
  summarize(Irrigation_m3_mean = mean(Irrigation_m3),
            Irrigation_m3_std = sd(Irrigation_m3),
            Irrigation_m3_min = min(Irrigation_m3),
            Irrigation_m3_max = max(Irrigation_m3))

# plot timeseries
p_timeseries <- 
  ggplot(df_allts, aes(x = Year, y = Irrigation_m3/1e7, color = Algorithm)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(a) Calendar Year (January-December)", 
                                      "Growing Season" = "(b) Growing Season (April-October)", 
                                      "Water Year" = "(c) Water Year (October-September)"))) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_y_continuous(name = "Annual Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

# plot barcharts
reported_avg <- df_allts_avg$Irrigation_m3_mean[df_allts_avg$Algorithm == "Reported" & df_allts_avg$ts == "Annual"]
reported_std <- df_allts_avg$Irrigation_m3_std[df_allts_avg$Algorithm == "Reported" & df_allts_avg$ts == "Annual"]
reported_min <- min(df_allts$Irrigation_m3[df_allts$Algorithm == "Reported" & df_allts$ts == "Annual"])
reported_max <- max(df_allts$Irrigation_m3[df_allts$Algorithm == "Reported" & df_allts$ts == "Annual"])

p_average_col <- 
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           #ymin = reported_avg*1.1/1e7, ymax = reported_avg*0.9/1e7,
           ymin = (reported_avg - reported_std)/1e7, ymax = (reported_avg + reported_std)/1e7,
           fill = col.gray) +
  geom_col(data = df_allts_avg, 
           aes(x = Algorithm, y = Irrigation_m3_mean/1e7, fill = Algorithm)) +
  geom_linerange(data = df_allts_avg, 
                 aes(x = Algorithm, color = Algorithm,
                     ymin = (Irrigation_m3_mean - Irrigation_m3_std)/1e7,
                     ymax = (Irrigation_m3_mean + Irrigation_m3_std)/1e7)) +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(d) Calendar Year (January-December)", 
                                      "Growing Season" = "(e) Growing Season (April-October)", 
                                      "Water Year" = "(f) Water Year (October-September)"))) +
  scale_fill_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_x_discrete(labels = labs_algorithms) +
  scale_y_continuous(name = "Five-Year Average Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 2))


p_average_pointrange <- 
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           #ymin = reported_avg*1.1/1e7, ymax = reported_avg*0.9/1e7,
           ymin = reported_min/1e7, ymax = reported_max/1e7,
           fill = col.gray) +
  #geom_col(data = df_allts_avg, 
  #         aes(x = Algorithm, y = Irrigation_m3_mean/1e7, fill = Algorithm)) +
  geom_pointrange(data = df_allts_avg, 
                  aes(x = Algorithm, color = Algorithm,
                      y = Irrigation_m3_mean/1e7,
                      ymin = Irrigation_m3_min/1e7,
                      ymax = Irrigation_m3_max/1e7)) +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(d) Calendar Year (January-December)", 
                                      "Growing Season" = "(e) Growing Season (April-October)", 
                                      "Water Year" = "(f) Water Year (October-September)"))) +
  scale_fill_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_x_discrete(labels = labs_algorithms) +
  scale_y_continuous(name = "Five-Year Average Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

p_boxplot5yr <- 
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           #ymin = reported_avg*1.1/1e7, ymax = reported_avg*0.9/1e7,
           ymin = reported_min/1e7, ymax = reported_max/1e7,
           fill = col.gray) +
  geom_boxplot(data = df_allts, aes(x = Algorithm, y = Irrigation_m3/1e7, fill = Algorithm)) +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(d) Calendar Year (January-December)", 
                                      "Growing Season" = "(e) Growing Season (April-October)", 
                                      "Water Year" = "(f) Water Year (October-September)"))) +
  scale_fill_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  #scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_x_discrete(name = "Model", labels = labs_algorithms) +
  scale_y_continuous(name = "Five-Year Average Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

# combine plots
(
  (p_timeseries + 
     guides(color = "none") +
     theme(axis.title.x = element_blank())) + 
    (p_average_pointrange + 
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 45, hjust = 1),
             axis.title.x = element_blank())) 
) + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "Fig3_CompareOpenET-WIMAS_LEMA.png"),
       width = 190, height = 145, units = "mm")

##### ACRE-FEET VERSION

# plot timeseries
p_timeseries_af <- 
  ggplot(df_allts, aes(x = Year, y = Irrigation_m3*0.000810714/1e3, color = Algorithm)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(a) Calendar Year (January-December)", 
                                      "Growing Season" = "(b) Growing Season (April-October)", 
                                      "Water Year" = "(c) Water Year (October-September)"))) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_y_continuous(name = "Annual Irrigation [x1000 acre-feet]",
                     breaks = seq(0, 60, 10)) +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 3))

# plot barcharts
p_average_af <- 
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           #ymin = reported_avg*1.1/1e7, ymax = reported_avg*0.9/1e7,
           ymin = (reported_avg - reported_std)*0.000810714/1e3, ymax = (reported_avg + reported_std)*0.000810714/1e3,
           fill = col.gray) +
  geom_col(data = df_allts_avg, 
           aes(x = Algorithm, y = Irrigation_m3_mean*0.000810714/1e3, fill = Algorithm)) +
  geom_linerange(data = df_allts_avg, 
                 aes(x = Algorithm, color = Algorithm,
                     ymin = (Irrigation_m3_mean - Irrigation_m3_std)*0.000810714/1e3,
                     ymax = (Irrigation_m3_mean + Irrigation_m3_std)*0.000810714/1e3)) +
  facet_wrap(~ts, ncol = 1, 
             labeller = as_labeller(c("Annual" = "(d) Calendar Year (January-December)", 
                                      "Growing Season" = "(e) Growing Season (April-October)", 
                                      "Water Year" = "(f) Water Year (October-September)"))) +
  scale_fill_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_x_discrete(labels = labs_algorithms) +
  scale_y_continuous(name = "Five-Year Average Irrigation [x1000 acre-feet]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 3))

# combine plots
(
  (p_timeseries_af + 
     guides(color = "none") +
     theme(axis.title.x = element_blank())) + 
    (p_average_af + 
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 45, hjust = 1),
             axis.title.x = element_blank())) 
) + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "Fig3_CompareOpenET-WIMAS_LEMA-AcreFeet.png"),
       width = 190, height = 150, units = "mm")


# calculate fit statistics
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]
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

write_csv(df_fit_wide, file.path("figures+tables", "Table1_Compare_OpenET-WIMAS_LEMA_allts_FitStats.csv"))

# pull in annual precip to plot fit vs. precipitation
timestep <- "Growing Season"
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_met <- 
  read_csv(file.path("data", paste0("gridmet_", gsub(" ", "", timestep), "ByField.csv"))) |> 
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
  geom_text(aes(label = Year), check_overlap = T, 
            hjust = c(1, 1, 0, 0.5, 0), 
            vjust = c(0.5, 0.5, 0.5, 0, 0.5), 
            nudge_x = c(-10, -10, 10, 8, 10),
            nudge_y = c(0, 0, 0, 0.10, 0.05)) +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms,
                     guide = NULL) +
  scale_x_continuous(name = "Growing Season Precipitation [mm]",
                     expand = expansion(mult = c(0.025, 0.07))) +
  scale_y_continuous(name = "(Calculated - Reported) Irrigation [x10\u2077 m\u00b3]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0))
ggsave(file.path("figures+tables", "Fig4_Compare_OpenET-WIMAS_LEMA_FitVsPrecip.png"),
       p_fit_precip, width = 95, height = 95, units = "mm")

p_fit_precip_af <-
  ggplot(subset(df_fit_with_precip, Algorithm == "ensemble"), aes(x = MeanPrecip_mm/25.4, 
                                                                  y = IrrDiff_m3*0.000810714/1e3, 
                                                                  color = Algorithm)) +
  geom_hline(yintercept = 0, color = col.gray) +
  stat_smooth(method = "lm") +
  geom_point() +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms,
                     guide = NULL) +
  scale_x_continuous(name = "Growing Season Precipitation [in]") +
  scale_y_continuous(name = "(Calculated - Reported) Irrigation [x1000 acre-feet]") +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0))
ggsave(file.path("figures+tables", "Fig4_Compare_OpenET-WIMAS_LEMA_FitVsPrecip_AcreFeet.png"),
       p_fit_precip_af, width = 95, height = 95, units = "mm")

summary(lm(IrrDiff_m3/1e7 ~ MeanPrecip_mm, data = subset(df_fit_with_precip, Algorithm == "ensemble")))

## statistical bias-correction of calculated irrigation using precip
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

# plot
p_timeseries_precipCorrect <- 
  ggplot(df_precipCorrect, aes(x = Year, y = Irrigation_m3_precipCorrect/1e7, color = Algorithm)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  scale_y_continuous(name = "Precipitation-Adjusted Annual Irrigation [x10\u2077 m\u00b3]", 
                     limits = c(min(df_precipCorrect$Irrigation_m3/1e7), max(df_precipCorrect$Irrigation_m3/1e7))) +
  theme(legend.position = "bottom",
        strip.text = element_text(hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

# combine with line fit plot and save
(p_fit_precip + p_timeseries_precipCorrect) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_level = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag.position = c(0.95, 0.97))
ggsave(file.path("figures+tables", "Fig4_Compare_OpenET-WIMAS_LEMA_FitVsPrecip+PrecipCorrect.png"),
       width = 190, height = 115, units = "mm")

# fit stats
df_fit_precipCorrect <-
  df_precipCorrect |> 
  subset(Algorithm != "Reported") |> 
  # have to pivot wider then longer to grab out the WIMAS data
  # calculate stats for ts, algorithm
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(Irrigation_m3_precipCorrect, Reported),
            MAE_1e7m3 = mae(Irrigation_m3_precipCorrect/1e7, Reported/1e7),
            R2 = getR2(Irrigation_m3_precipCorrect, Reported),
            slope = getSlope(Irrigation_m3_precipCorrect, Reported))

write_csv(df_fit_precipCorrect, file.path("figures+tables", "TableS1_Compare_OpenET-WIMAS_LEMA_allts_FitStats-PrecipCorrect.csv"))

 ## bar chart of some stats by timescale
ggplot(df_fit_long, aes(x = Algorithm, y = fit, fill = ts)) +
  geom_col(position = "dodge") +
  facet_wrap(~metric, nrow = 2, scales = "free_x",
             labeller = as_labeller(c("Bias_prc" = "Bias [%]",
                                      "MAE_1e7m3" = "MAE [x10\u2077 m\u00b3]",
                                      "R2" = "R\u00b2",
                                      "slope" = "Slope"))) +
  scale_fill_discrete(name = "Aggregation Timescale") +
  scale_x_discrete(name = "Model", labels = labs_algorithms) +
  scale_y_continuous(name = "Fit Statistic") +
  coord_flip() +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "FigS6_Compare_OpenET-WIMAS_LEMA_allts_FitStats_BarChart.png"),
       width = 190, height = 140, units = "mm")
