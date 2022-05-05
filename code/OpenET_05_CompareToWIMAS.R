## OpenET_05_CompareToWIMAS.R
# This script is supposed to get total irrigation within SD-6 LEMA 
# and compare to WIMAS output.

source(file.path("code", "paths+packages.R"))

## load data
# field attributes
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
irr_fields <- readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) # 1984-2020
met_fields <- readr::read_csv(file.path("data", "gridmet_AnnualByField.csv"))

# et data
df_et_yr <- readr::read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Annual_All_FieldsNoDups.csv")) # 2016-2021

# wimas data
wimas_wuse <- readr::read_csv(file.path(dir_data, "WIMAS_WaterUse_1990to2020_SD6+10mi_Summarize.csv"))

# join together
lema_fields_data <-
  df_et_yr %>% 
  left_join(fields_spatial, by = "UID") %>% 
  subset(within_lema) %>% 
  left_join(irr_fields, by = c("UID", "Year")) %>% 
  left_join(met_fields, by = c("UID", "Year"))

# set irrigation threshold and subset to irrigated fields only
irr_thres <- 0.5 # 0-1
lema_irr_fields <- 
  subset(lema_fields_data, IrrigatedPrc > irr_thres)

# calculate volumetric irrigation per field
lema_irr_fields$Irr_mm <- lema_irr_fields$ET_mm - lema_irr_fields$precip_mm
lema_irr_fields$Irr_mm[lema_irr_fields$Irr_mm < 0] <- 0
lema_irr_fields$Irr_m3 <- lema_irr_fields$area_m2*lema_irr_fields$Irr_mm/1000

# sum by year and algorithm
lema_irr_totals <-
  lema_irr_fields %>% 
  group_by(Year, Algorithm) %>% 
  summarize(Irr_m3_total = sum(Irr_m3),
            n_fields = n())

# combine with wimas
irr_openet_wimas <-
  left_join(lema_irr_totals, wimas_wuse, by = "Year") %>% 
  rename(Irr_m3_OpenET = Irr_m3_total, Irr_m3_WIMAS = TotalVolume_m3)

# save
write_csv(irr_openet_wimas, file.path("data", "OpenET_05_CompareToWIMAS_AnnualTotals.csv"))

# plot
p_scatter <- 
  ggplot(irr_openet_wimas, aes(x = Irr_m3_OpenET/1e6, y = Irr_m3_WIMAS/1e6)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = Algorithm)) +
  scale_x_continuous(name = "OpenET Estimated Irrigation [million m\u00b3]") +
  scale_y_continuous(name = "WIMAS Reported Irrigation [million m\u00b3]") +
  scale_color_brewer(name = "Algorithm", type = "qual", labels = labs_algorithms) +
  stat_smooth(method = "lm") +
  theme(legend.position = "bottom")

p_line <-
  ggplot(irr_openet_wimas) +
  # WIMAS data
  geom_point(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  geom_line(aes(x = Year, y = Irr_m3_WIMAS/1e6), color = "black", size = 2) +
  # OpenET data
  geom_point(aes(x = Year, y = Irr_m3_OpenET/1e6, color = Algorithm)) + 
  geom_line(aes(x = Year, y = Irr_m3_OpenET/1e6, color = Algorithm), show.legend = F) +
  # aesthetics
  scale_y_continuous(name = "Irrigation [million m\u00b3]") +
  scale_color_brewer(name = "Algorithm", type = "qual", labels = labs_algorithms) +
  theme(legend.position = "bottom")

p_combo <-
  p_scatter + p_line +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
p_combo

ggsave(file.path("plots", "OpenET_05_CompareToWIMAS.png"),
       p_combo, width = 190, height = 120, units = "mm")
