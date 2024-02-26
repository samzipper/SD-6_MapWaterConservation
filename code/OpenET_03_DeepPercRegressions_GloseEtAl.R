## DeepPercRegressions_GloseEtAl.R
# This script usesdata from Glose et al (2022): https://osf.io/8vmkg
# Plots the relationship between precipitation and deep percolation.
# This can be used to calculate effective precipitation.

source(file.path("code", "paths+packages.R"))

# load data
df <- read_csv(file.path("data", "GloseEtAl-PPT_Relationship_Data.csv"))
names(df)

# load gridmet precip - so we have annual, growing season, and water year
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_met_gs <- 
  read_csv(file.path("data", "gridmet_GrowingSeasonByField.csv")) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema) |> 
  group_by(Year) |> 
  summarize(MeanPrecip_mm = mean(precip_mm)) |> 
  mutate(ts = "Growing Season")
fields_met_wyr <- 
  read_csv(file.path("data", "gridmet_WaterYearByField.csv")) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema) |> 
  rename(Year = WaterYear) |> 
  group_by(Year) |> 
  summarize(MeanPrecip_mm = mean(precip_mm)) |> 
  mutate(ts = "Water Year")
fields_met_yr <- 
  read_csv(file.path("data", "gridmet_AnnualByField.csv")) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema) |> 
  group_by(Year) |> 
  summarize(MeanPrecip_mm = mean(precip_mm)) |> 
  mutate(ts = "Calendar Year")

fields_met <- bind_rows(fields_met_gs, fields_met_wyr, fields_met_yr)

df_withMet <- left_join(fields_met, df, by = "Year")


ggplot(df_withMet, aes(x = MeanPrecip_mm, y = `Ann LEMA DP (mm)`)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ts) +
  labs(x = "Total Precipitation [mm]",
       y = "Annual Deep Percolation [mm]")
ggsave(file.path("figures+tables", "Fig_DeepPerc-GloseEtAl.png"),
       width = 190, height = 95, units = "mm")

lm_dpVprecip_yr <- lm(`Ann LEMA DP (mm)` ~ MeanPrecip_mm, data = subset(df_withMet, ts == "Calendar Year"))
lm_dpVprecip_wyr <- lm(`Ann LEMA DP (mm)` ~ MeanPrecip_mm, data = subset(df_withMet, ts == "Water Year"))
lm_dpVprecip_gs <- lm(`Ann LEMA DP (mm)` ~ MeanPrecip_mm, data = subset(df_withMet, ts == "Growing Season"))

summary(lm_dpVprecip_yr)
summary(lm_dpVprecip_wyr)
summary(lm_dpVprecip_gs)

# compile linear regression coefficients
df_lm <- 
  tibble(ts = c("Annual", "WaterYear", "GrowingSeason"),
         lmInt = c(coef(lm_dpVprecip_yr)[1], coef(lm_dpVprecip_wyr)[1], coef(lm_dpVprecip_gs)[1]),
         lmSlope = c(coef(lm_dpVprecip_yr)[2], coef(lm_dpVprecip_wyr)[2], coef(lm_dpVprecip_gs)[2]),
         r2 = c(summary(lm_dpVprecip_yr)$r.squared, summary(lm_dpVprecip_wyr)$r.squared, summary(lm_dpVprecip_gs)$r.squared))
write_csv(df_lm, file.path("data", "DeepPercRegressions_Summary.csv"))
