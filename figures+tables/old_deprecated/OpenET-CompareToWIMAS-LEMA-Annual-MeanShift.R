## OpenET-CompareToWIMAS-LEMA-Annual-MeanShift.R
# This script is supposed to get total irrigation within SD-6 LEMA 
# and compare to WIMAS output. Uses irrigation estimates based on 
# annual ET and met data and shifts the mean based on pumping allocations.

source(file.path("code", "paths+packages.R"))

## load data
# field attributes
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

# et data
df_et_yr <- 
  readr::read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "OpenET_EstimateFieldIrrigation-Annual_FieldsNoDups.csv")) |>  # 2016-2021
  # re-calculate here so that you allow negative values (full distribution, not cut off at 0)
  mutate(irr_mm_fromPrec = ET_mm - precip_mm) |> 
  select(-irr_m3_fromPrec, -irr_mm_fromNonIrr, -irr_m3_fromNonIrr)

# wimas data
wimas_wuse <- readr::read_csv(file.path(dir_data, "WIMAS_WaterUse_1990to2020_SD6+10mi_Summarize.csv"))

# join together
lema_fields_data <-
  df_et_yr |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema)

# set irrigation threshold and subset to irrigated fields only
irr_thres <- 0.5 # 0-1
lema_irr_fields <- 
  subset(lema_fields_data, IrrigatedPrc > irr_thres)

## work on shifting mean to match up with allocated amount
allocated_mm_yr <- 11*25.4 # target: 11"; Deines et al. estimated, 9.6"
elasticity_mm_yr <- 3*25.4 # 3" +/- should be 5th and 95th percentiles

# sum to total irrigation depth over 5 years
lema_irr_fields_5yr <-
  lema_irr_fields |> 
  group_by(UID, Algorithm) |> 
  summarize(irr_mm_fromPrec_5yrSum = sum(irr_mm_fromPrec),
            irr_mm_fromPrec_5yrMean = mean(irr_mm_fromPrec),
            n_yrs = n())

ggplot(lema_irr_fields_5yr, aes(x = irr_mm_fromPrec_5yrMean, color = Algorithm)) +
  geom_vline(xintercept = allocated_mm_yr) + # 11 inches
  geom_density()

# calculate bias
algorithm_shift <-
  lema_irr_fields_5yr |> 
  group_by(Algorithm) |> 
  summarize(mean_bias_mm = mean(irr_mm_fromPrec_5yrMean - allocated_mm_yr))

# shift
lema_irr_fields_withShift <-
  lema_irr_fields |> 
  left_join(algorithm_shift, by = "Algorithm") |> 
  mutate(irr_mm_fromPrec_shifted = irr_mm_fromPrec - mean_bias_mm)

ggplot(lema_irr_fields_withShift, aes(x = irr_mm_fromPrec, color = Algorithm)) +
  annotate("rect", ymin = -Inf, ymax = Inf,
           xmin = (allocated_mm_yr - elasticity_mm_yr), 
           xmax = (allocated_mm_yr + elasticity_mm_yr),
           fill = col.cat.blu, alpha = 0.5) +
  geom_density()

ggplot(lema_irr_fields_withShift, aes(x = irr_mm_fromPrec_shifted, color = Algorithm)) +
  annotate("rect", ymin = -Inf, ymax = Inf,
           xmin = (allocated_mm_yr - elasticity_mm_yr), 
           xmax = (allocated_mm_yr + elasticity_mm_yr),
           fill = col.cat.blu, alpha = 0.5) +
  geom_density()

# calculate 5th/95th percentiles to scale elasticity
algorithm_year_bounds <- 
  lema_irr_fields_withShift |> 
  group_by(Algorithm, Year) |> 
  summarize(irr_mm_fromPrec_shifted_lower = quantile(irr_mm_fromPrec_shifted, 0.05),
            irr_mm_fromPrec_shifted_upper = quantile(irr_mm_fromPrec_shifted, 0.95))

# scale
lema_irr_fields_withShiftScale <-
  lema_irr_fields_withShift |> 
  left_join(algorithm_year_bounds, by = c("Algorithm", "Year")) |> 
  mutate(irr_mm_fromPrec_shifted_scaled = (allocated_mm_yr - elasticity_mm_yr) + 
           ((allocated_mm_yr + elasticity_mm_yr) - (allocated_mm_yr - elasticity_mm_yr))*
           (irr_mm_fromPrec_shifted-irr_mm_fromPrec_shifted_lower)/(irr_mm_fromPrec_shifted_upper-irr_mm_fromPrec_shifted_lower))

ggplot(lema_irr_fields_withShiftScale, aes(x = irr_mm_fromPrec_shifted_scaled, color = Algorithm)) +
  annotate("rect", ymin = -Inf, ymax = Inf,
           xmin = (allocated_mm_yr - elasticity_mm_yr), 
           xmax = (allocated_mm_yr + elasticity_mm_yr),
           fill = col.cat.blu, alpha = 0.5) +
  geom_density()

# adjust anything <0 = 0
lema_irr_fields_withShiftScale$irr_mm_fromPrec[lema_irr_fields_withShiftScale$irr_mm_fromPrec < 0] <- 0
lema_irr_fields_withShiftScale$irr_mm_fromPrec_shifted[lema_irr_fields_withShiftScale$irr_mm_fromPrec_shifted < 0] <- 0
lema_irr_fields_withShiftScale$irr_mm_fromPrec_shifted_scaled[lema_irr_fields_withShiftScale$irr_mm_fromPrec_shifted_scaled < 0] <- 0

# sum by year and algorithm
lema_irr_totals <-
  lema_irr_fields_withShiftScale |> 
  group_by(Year, Algorithm) |> 
  summarize(irr_m3_fromPrec_total = sum(area_m2*irr_mm_fromPrec/1000),
            irr_m3_fromPrec_shifted_total = sum(area_m2*irr_mm_fromPrec_shifted/1000),
            irr_m3_fromPrec_shifted_scaled_total = sum(area_m2*irr_mm_fromPrec_shifted_scaled/1000),
            n_fields = n())

# combine with wimas
irr_openet_wimas <-
  left_join(lema_irr_totals, wimas_wuse, by = "Year") |> 
  rename(Irr_m3_WIMAS = TotalVolume_m3)
write_csv(irr_openet_wimas, file.path("figures+tables", "OpenET-CompareToWIMAS-LEMA-Annual_TotalIrrigation.csv"))


irr_openet_wimas_long <-
  irr_openet_wimas |> 
  pivot_longer(cols = starts_with("irr_m3_fromPrec"),
               names_to = "Processing",
               values_to = "Irr_m3_OpenET") |> 
  mutate(Processing = factor(Processing, 
                             levels = c("irr_m3_fromPrec_total", "irr_m3_fromPrec_shifted_total", "irr_m3_fromPrec_shifted_scaled_total"),
                             labels = c("ET - P", "Bias-Corrected", "Bias-Corrected + Scaled")))

# inspect fit
p_timeseries <-
  ggplot(irr_openet_wimas_long) +
  # WIMAS data
  geom_point(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  geom_line(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  # OpenET data
  geom_point(aes(x = Year, y = Irr_m3_OpenET/1e6, color = Algorithm)) + 
  geom_line(aes(x = Year, y = Irr_m3_OpenET/1e6, color = Algorithm), show.legend = F) +
  # facet
  facet_wrap(. ~ Processing, ncol = 1) +
  # aesthetics
  scale_y_continuous(name = "Estimated Irrigation [million m\u00b3]") +
  scale_color_brewer(name = NULL, type = "qual", labels = labs_algorithms) +
  theme(legend.position = "bottom")
p_timeseries

# make final plots
p_scatter_fromPrec <- 
  ggplot(irr_openet_wimas, aes(y = irr_m3_fromPrec_total/1e6, x = Irr_m3_WIMAS/1e6)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = Algorithm)) +
  scale_y_continuous(name = "Estimated Irrigation\n[million m\u00b3]", 
                     limits = c(min(irr_openet_wimas$irr_m3_fromPrec_total/1e6), 
                                max(irr_openet_wimas$irr_m3_fromPrec_total/1e6))) +
  scale_x_continuous(name = "WIMAS Reported Irrigation [million m\u00b3]", 
                     limits = c(min(irr_openet_wimas$irr_m3_fromPrec_total/1e6), 
                                max(irr_openet_wimas$irr_m3_fromPrec_total/1e6))) +
  scale_color_brewer(name = "Algorithm", type = "qual", labels = labs_algorithms) +
  stat_smooth(method = "lm") +
  theme(legend.position = "bottom")
p_scatter_fromPrec

p_line_fromPrec <-
  ggplot(irr_openet_wimas) +
  # WIMAS data
  geom_line(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  geom_point(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  # OpenET data
  geom_line(aes(x = Year, y = irr_m3_fromPrec_total/1e6, color = Algorithm), show.legend = F) +
  geom_point(aes(x = Year, y = irr_m3_fromPrec_total/1e6, color = Algorithm)) + 
  # aesthetics
  scale_y_continuous(name = "Estimated Irrigation\n[million m\u00b3]") +
  scale_color_brewer(name = "Algorithm", type = "qual", labels = labs_algorithms) +
  theme(legend.position = "bottom")
p_line_fromPrec

p_scatter_fromPrec_shifted <- 
  ggplot(irr_openet_wimas, aes(y = irr_m3_fromPrec_shifted_total/1e6, x = Irr_m3_WIMAS/1e6)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = Algorithm)) +
  scale_y_continuous(name = "Allocation-Shifted\nIrrigation [million m\u00b3]", 
                     limits = layer_scales(p_scatter_fromPrec)$y$get_limits()) +
  scale_x_continuous(name = "WIMAS Reported Irrigation [million m\u00b3]", 
                     limits = layer_scales(p_scatter_fromPrec)$x$get_limits()) +
  scale_color_brewer(name = "Algorithm", type = "qual", labels = labs_algorithms) +
  stat_smooth(method = "lm") +
  theme(legend.position = "bottom")
p_scatter_fromPrec_shifted

p_line_fromPrec_shifted <-
  ggplot(irr_openet_wimas) +
  # WIMAS data
  geom_line(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  geom_point(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  # OpenET data
  geom_line(aes(x = Year, y = irr_m3_fromPrec_shifted_total/1e6, color = Algorithm), show.legend = F) +
  geom_point(aes(x = Year, y = irr_m3_fromPrec_shifted_total/1e6, color = Algorithm)) + 
  # aesthetics
  scale_y_continuous(name = "Allocation-Shifted\nIrrigation [million m\u00b3]", 
                     limits = layer_scales(p_line_fromPrec)$y$get_limits()) +
  scale_color_brewer(name = "Algorithm", type = "qual", labels = labs_algorithms) +
  theme(legend.position = "bottom")
p_line_fromPrec_shifted

# make combo and save
p_combo <- 
  (p_line_fromPrec + p_scatter_fromPrec +
  p_line_fromPrec_shifted + p_scatter_fromPrec_shifted) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
p_combo

# save
ggsave(file.path("figures+tables", "OpenET-CompareToWIMAS-LEMA-Annual-MeanShift_IrrFromPrecip.png"),
       p_combo, width = 17.15, height = 12.15, units = "cm")