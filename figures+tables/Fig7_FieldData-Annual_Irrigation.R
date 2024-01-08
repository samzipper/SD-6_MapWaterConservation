## FieldData-Annual_02_EstimateIrrigation.R

source(file.path("code", "paths+packages.R"))

# load all fields and remove data you don't want
df_all <- read_csv(file.path(dir_farm_data, "FieldData_AllFieldsCompiled-Annual.csv"))

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

# estimate irrigation as ET-P
df_long$ET.P_mm <- df_long$ET_mm - df_long$precip_mm
df_long$irrEst_mm <- ifelse(df_long$ET.P_mm >= 0, df_long$ET.P_mm, 0)
df_long$P.Irr_mm <- df_long$precip_mm + df_long$irrigation_mm

# calculate cumulative irrigation
df_long_cumirr <- 
  df_long |> 
  group_by(FieldID, Region, Algorithm) |> 
  reframe(irrigation_mm_cumsum = cumsum(irrigation_mm),
          irrEst_mm_cumsum = cumsum(irrEst_mm),
          P.Irr_mm_cumsum = cumsum(P.Irr_mm),
          ET_mm_cumsum = cumsum(ET_mm),
          ETo_mm_cumsum = cumsum(ETo_mm)) |> 
  mutate(Year = df_long$Year)

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
         irrEst_mm_mean = irrEst_mm_cumsum/n_years)

# corn only - for plots
df_corn <- subset(df_long, cropType %in% c("Corn", "Corn-Grain", "Corn-Corn"))

## plots
# mm version
p_annual_mm <-
  ggplot(df_corn, aes(x = irrEst_mm, y = irrigation_mm)) +
  geom_abline(color = col.gray) +
  geom_point(aes(color = Region), shape = 1) +
  #stat_smooth(method = "lm") +
  scale_x_continuous(name = "Estimated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Reported Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02")) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms))

p_avg_mm <-
  ggplot(data = df_multiyr_mean, aes(x = irrEst_mm_mean, y = irrigation_mm_mean)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(n_years)), shape = 1) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Avg. Estimated Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  scale_y_continuous(name = "Avg. Rep Irrigation [mm]",
                     limits = c(0, 850),
                     breaks = seq(0, 700, 350)) +
  coord_equal() +
  scale_color_manual(name = "Number of Years", values = c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e"))

(p_annual_mm + p_avg_mm) +
  plot_layout(nrow = 2) & #+
  #plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        legend.margin = margin(t = -15))
ggsave(file.path("figures+tables", "Fig7_FieldData-Annual_Irrigation_mm.png"),
       width = 190, height = 100, units = "mm")

# inches version
p_annual_in <-
  ggplot(df_corn, aes(x = irrEst_mm/25.4, y = irrigation_mm/25.4)) +
  geom_abline(color = col.gray) +
  geom_point(aes(color = Region), shape = 1) +
  #stat_smooth(method = "lm") +
  scale_x_continuous(name = "Estimated Irrigation [in]",
                     limits = c(0, 34)) +
  scale_y_continuous(name = "Reported Irrigation [in]",
                     limits = c(0, 34)) +
  coord_equal() +
  scale_color_manual(name = "Region", 
                     values = c("NC" = "#7570b3", 
                                "WC" = "#e7298a", 
                                "NW" = "#1b9e77",
                                "SW" = "#d95f02")) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms))

p_avg_in <-
  ggplot(data = df_multiyr_mean, aes(x = irrEst_mm_mean/25.4, y = irrigation_mm_mean/25.4)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = factor(n_years)), shape = 1) +
  facet_wrap(~Algorithm, nrow = 1, labeller = as_labeller(labs_algorithms)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Avg. Estimated Irrigation [in]",
                     limits = c(0, 31)) +
  scale_y_continuous(name = "Avg. Rep Irrigation [in]",
                     limits = c(0, 31)) +
  coord_equal() +
  scale_color_manual(name = "Number of Years", values = c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e"))

(p_annual_in + p_avg_in) +
  plot_layout(nrow = 2) & #+
  #plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        legend.margin = margin(t = -15))
ggsave(file.path("figures+tables", "Fig7_FieldData-Annual_Irrigation_Inches.png"),
       width = 190, height = 100, units = "mm")

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
