## FieldData-Annual_Irrigation.R

source(file.path("code", "paths+packages.R"))

# load all fields and remove data you don't want
df_all <- read_csv(file.path(dir_farm_data, "FieldData_AllFieldsCompiled-Annual.csv"))

# load deep percolation regressions
df_lm <- read_csv(file.path("data", "DeepPercRegressions_Summary.csv")) # DP = lmSlope*AnnualPrecip_mm + lmInt

# pivot to long form
df_long <- 
  df_all |> 
  pivot_longer(ends_with("_et_mm"),
               names_to = "Algorithm", values_to = "ET_mm") |> 
  arrange(FieldID, Algorithm) |> 
  mutate(Algorithm = str_sub(Algorithm, start = 1, end = -7),
         Region = str_sub(FieldID, 1, 2))

# lump the NB field in with NW (just over the border)
df_long$Region[df_long$Region == "NB"] <- "NW"

# calculate effective precipitation
df_long$DeepPerc_mm <- df_lm$lmSlope[df_lm$ts == "Annual"]*df_long$precip_mm + df_lm$lmInt[df_lm$ts == "Annual"]
df_long$DeepPerc_mm[df_long$DeepPerc_mm < 0] <- 0
df_long$Peff_mm <- df_long$precip_mm - df_long$DeepPerc_mm

# estimate irrigation as ET-P
df_long$ET.P_mm <- df_long$ET_mm - df_long$precip_mm
df_long$ET.Peff_mm <- df_long$ET_mm - df_long$Peff_mm
df_long$irrEst_mm <- ifelse(df_long$ET.P_mm >= 0, df_long$ET.P_mm, 0)
df_long$irrEstPeff_mm <- ifelse(df_long$ET.Peff_mm >= 0, df_long$ET.Peff_mm, 0)
df_long$P.Irr_mm <- df_long$precip_mm + df_long$irrigation_mm

# calculate cumulative irrigation
df_long_cumirr <- 
  df_long |> 
  group_by(FieldID, Region, Algorithm) |> 
  reframe(irrigation_mm_cumsum = cumsum(irrigation_mm),
          irrEst_mm_cumsum = cumsum(irrEst_mm),
          irrEstPeff_mm_cumsum = cumsum(irrEstPeff_mm),
          P.Irr_mm_cumsum = cumsum(P.Irr_mm),
          ET_mm_cumsum = cumsum(ET_mm),
          ETo_mm_cumsum = cumsum(ETo_mm)) |> 
  mutate(Year = df_long$Year)

sum(is.finite(df_long$irrigation_mm)/7)
sum(is.finite(df_long$irrEst_mm)/7)
sum(is.finite(df_long$irrEstPeff_mm)/7)
length(unique(df_long$FieldID))

# choose only sites with >= 4 yrs (50%) data
fields_multiyr <- 
  df_all |> 
  group_by(FieldID) |> 
  summarize(n_years = n()) |> 
  subset(n_years >= 4)

df_multiyr <- subset(df_long_cumirr, FieldID %in% fields_multiyr$FieldID)

# average across multi-year period
df_multiyr_mean <-
  df_multiyr |> 
  group_by(FieldID, Algorithm) |> 
  filter(Year == max(Year)) |> 
  left_join(fields_multiyr, by = "FieldID") |> 
  mutate(irrigation_mm_mean = irrigation_mm_cumsum/n_years,
         irrEst_mm_mean = irrEst_mm_cumsum/n_years,
         irrEstPeff_mm_mean = irrEstPeff_mm_cumsum/n_years)

## plots
# mm version (using P)
p_annual_mm <-
  ggplot(df_long, aes(x = irrEst_mm, y = irrigation_mm)) +
  geom_abline(color = col.gray) +
  geom_point(aes(color = factor(Region, levels = c("WC", "NW", "SW", "NC"))), shape = 1) +
  #stat_smooth(method = "lm") +
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Reported\nIrrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02"),
                     drop = F) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms))

p_avg_mm <-
  ggplot(data = df_multiyr_mean, aes(x = irrEst_mm_mean, y = irrigation_mm_mean)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Region, levels = c("WC", "NW", "SW", "NC"))), shape = 1) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Avg. Calculated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Avg. Reported\nIrrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02"),
                     drop = F)

(p_annual_mm + p_avg_mm) +
  plot_layout(nrow = 2,
              guides = "collect") & #+
  #plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        legend.margin = margin(t = -5))
ggsave(file.path("figures+tables", "Fig7_FieldData-Annual_Irrigation_mm.png"),
       width = 190, height = 85, units = "mm")

# fit stats
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]
df_fit_annual <-
  df_long |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(irrEst_mm, irrigation_mm),
            MAE_mm = mae(irrEst_mm, irrigation_mm),
            R2 = getR2(irrEst_mm, irrigation_mm),
            slope = getSlope(irrEst_mm, irrigation_mm)) |> 
  mutate(time = "Annual")

df_fit_avg <-
  df_multiyr_mean |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(irrEst_mm_mean, irrigation_mm_mean),
            MAE_mm = mae(irrEst_mm_mean, irrigation_mm_mean),
            R2 = getR2(irrEst_mm_mean, irrigation_mm_mean),
            slope = getSlope(irrEst_mm_mean, irrigation_mm_mean)) |> 
  mutate(time = "Average")

summary(lm(irrigation_mm_mean ~ irrEst_mm_mean, data = subset(df_multiyr_mean, Algorithm == "ssebop")))

df_fit_both <-
  bind_rows(df_fit_annual, df_fit_avg)
write_csv(df_fit_both, file.path("figures+tables", "Table2_FieldDataFitStats.csv"))

# mm version (using Peff)
p_annual_mm_Peff <-
  ggplot(df_long, aes(x = irrEstPeff_mm, y = irrigation_mm)) +
  geom_abline(color = col.gray) +
  geom_point(aes(color = factor(Region, levels = c("WC", "NW", "SW", "NC"))), shape = 1) +
  #stat_smooth(method = "lm") +
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Reported\nIrrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02"),
                     drop = F) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms))

p_avg_mm_Peff <-
  ggplot(data = df_multiyr_mean, aes(x = irrEstPeff_mm_mean, y = irrigation_mm_mean)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Region, levels = c("WC", "NW", "SW", "NC"))), shape = 1) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Avg. Calculated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Avg. Reported\nIrrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02"),
                     drop = F)

(p_annual_mm_Peff + p_avg_mm_Peff) +
  plot_layout(nrow = 2,
              guides = "collect") & #+
  #plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        legend.margin = margin(t = -5))
ggsave(file.path("figures+tables", "Fig7_Peff_FieldData-Annual_Irrigation_mm.png"),
       width = 190, height = 90, units = "mm")

# fit stats
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]
df_fit_annual_Peff <-
  df_long |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(irrEstPeff_mm, irrigation_mm),
            MAE_mm = mae(irrEstPeff_mm, irrigation_mm),
            R2 = getR2(irrEstPeff_mm, irrigation_mm),
            slope = getSlope(irrEstPeff_mm, irrigation_mm)) |> 
  mutate(time = "Annual")

df_fit_avg_Peff <-
  df_multiyr_mean |> 
  group_by(Algorithm) |> 
  summarize(Bias_prc = pbias(irrEstPeff_mm_mean, irrigation_mm_mean),
            MAE_mm = mae(irrEstPeff_mm_mean, irrigation_mm_mean),
            R2 = getR2(irrEstPeff_mm_mean, irrigation_mm_mean),
            slope = getSlope(irrEstPeff_mm_mean, irrigation_mm_mean)) |> 
  mutate(time = "Average")

summary(lm(irrigation_mm_mean ~ irrEstPeff_mm_mean, data = subset(df_multiyr_mean, Algorithm == "ssebop")))

df_fit_both_Peff <-
  bind_rows(df_fit_annual_Peff, df_fit_avg_Peff)
write_csv(df_fit_both_Peff, file.path("figures+tables", "Table2_Peff_FieldDataFitStats.csv"))

# inches version (using P)
p_annual_in <-
  ggplot(df_long, aes(x = irrEst_mm/25.4, y = irrigation_mm/25.4)) +
  geom_abline(color = col.gray) +
  geom_point(aes(color = factor(Region, levels = c("WC", "NW", "SW", "NC"))), shape = 1) +
  #stat_smooth(method = "lm") +
  scale_x_continuous(name = "Calculated Irrigation [in]",
                     limits = c(0, 34)) +
  scale_y_continuous(name = "Reported Irrigation [in]",
                     limits = c(0, 34)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02"),
                     drop = F) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms))

p_avg_in <-
  ggplot(data = df_multiyr_mean, aes(x = irrEst_mm_mean/25.4, y = irrigation_mm_mean/25.4)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(Region, levels = c("WC", "NW", "SW", "NC"))), shape = 1) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Avg. Calculated Irrigation [in]",
                     limits = c(0, 31)) +
  scale_y_continuous(name = "Avg. Rep Irrigation [in]",
                     limits = c(0, 31)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02"),
                     drop = F)

(p_annual_in + p_avg_in) +
  plot_layout(nrow = 2, guides = "collect") & #+
  #plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        legend.margin = margin(t = -5))
ggsave(file.path("figures+tables", "Fig7_FieldData-Annual_Irrigation_Inches.png"),
       width = 190, height = 85, units = "mm")




# plot cumulative sum through time
ggplot(data = df_multiyr, aes(x = Year)) +
  geom_line(aes(y = irrEst_mm_cumsum/25.4, color = Algorithm)) +
  geom_point(aes(y = irrigation_mm_cumsum/25.4), shape = 1, color = "black") +
  facet_wrap(~FieldID, nrow = 4) +
  scale_x_continuous(breaks = seq(2016, 2022, 3)) +
  scale_y_continuous(name = "Cumulative Irrigation [in]") +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  theme(legend.position = "bottom")
ggsave(file.path("plots", "FieldData-Annual_Cumulative.png"),
       width = 150, height = 210, units = "mm")


## graphical abstract version
p_ga_annual <-
  ggplot(subset(df_corn, Algorithm == "ensemble"), aes(x = irrEst_mm, y = irrigation_mm)) +
  geom_abline(color = col.gray) +
  geom_point(shape = 1, color = "#e31a1c") +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  labs(title = "Individual Years") +
  coord_equal() +
  theme(plot.title = element_text(face = "italic", hjust = 0.5))

p_ga_avg <- 
  ggplot(data = subset(df_multiyr_mean, Algorithm == "ensemble"), aes(x = irrEst_mm_mean, y = irrigation_mm_mean)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1, color = "#e31a1c") +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Calculated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  labs(title = "Multi-Year Average") +
  coord_equal() +
  theme(plot.title = element_text(face = "italic", hjust = 0.5))


(p_ga_annual + p_ga_avg) +
  plot_layout(nrow = 1)
ggsave(file.path("figures+tables", "Fig7-GA_FieldData-Annual_Irrigation.png"),
       width = 120, height = 60, units = "mm")
