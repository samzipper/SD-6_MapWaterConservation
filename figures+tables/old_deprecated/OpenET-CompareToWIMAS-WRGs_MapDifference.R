## OpenET-CompareToWIMAS-WRGs_MapDifference.R

source(file.path("code", "paths+packages.R"))

## set parameters for script
ts <- "WaterYear"  # timescale; Annual, GrowingSeason, WaterYear
alg <- "ensemble"  # algorithm to use

## load data
# field attributes
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

# lema boundary
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))

# wrgs
alldata_wrg <- readr::read_csv(file.path("data", paste0("WRgroups_Summarize-", ts, ".csv")))

# et data
df_et_yr <- 
  readr::read_csv(file.path(dir_data, "OpenET", "Monthly_2016-2021", paste0("OpenET_EstimateFieldIrrigation-", ts, "_FieldsNoDups.csv"))) |>  # 2016-2021
  # re-calculate here so that you allow negative values (full distribution, not cut off at 0)
  mutate(irr_mm_fromPrec = ET_mm - precip_mm) |> 
  select(-irr_m3_fromPrec, -irr_mm_fromNonIrr, -irr_m3_fromNonIrr)

# wimas data
wimas_wuse <- readr::read_csv(file.path(dir_data, "WIMAS_WaterUse_1990to2020_SD6+10mi_Summarize.csv"))

# spatial data
sf_pdiv <- st_read(file.path(dir_data, "WIMAS_WaterUse_1990to2020_SD6+10mi.gpkg"))
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))

# join together
lema_fields_data <-
  df_et_yr |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema)

# set criteria for trimming to only the best matches
wrg_best <-
  alldata_wrg |> 
  subset(n_irrfields == 1 &
           n_IrrConfHigh == 1 &
           abs(IrrArea_m2 - area_m2_wrg)/area_m2_wrg < 0.1) |> 
  subset(Algorithm == alg)

# compare wrg irrigation volume to pumped volume
ggplot(wrg_best, aes(x = irr_m3_fromPrec/1e5, y = irr_m3_fromWIMAS/1e5, color = factor(Year))) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_wrap(~Algorithm, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [x10^5 m\u00b3]") +
  scale_y_continuous(name = "Reported Irrigation [x10^5 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.blu, "2018" = col.cat.grn))

# rank from best to worst
wrg_best$irr_m3_diff <- wrg_best$irr_m3_fromPrec - wrg_best$irr_m3_fromWIMAS  # positive means OpenET overestimates

yrs_all <- unique(wrg_best$Year)
for (yr in yrs_all){
  wrg_yr <-
    wrg_best |> 
    subset(Year == yr) |> 
    arrange(abs(irr_m3_diff)) |> 
    mutate(diff_rank = seq(1, length(irr_m3_diff)))
    
  if (yr == yrs_all[1]){
    wrg_ranked <- wrg_yr
  } else {
    wrg_ranked <- bind_rows(wrg_ranked, wrg_yr)
  }
}

# check if ranks make sense
ggplot(wrg_ranked, aes(x = irr_m3_fromPrec/1e5, y = irr_m3_fromWIMAS/1e5, color = factor(Year), size = diff_rank)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_wrap(~Algorithm, labeller = as_labeller(labs_algorithms)) +
  scale_x_continuous(name = "Estimated Irrigation [x10^5 m\u00b3]") +
  scale_y_continuous(name = "Reported Irrigation [x10^5 m\u00b3]") +
  scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.blu, "2018" = col.cat.grn))

# pull in fields and PDIVs by WRG
wrg_fields <- read_csv(file.path("data", "WRgroups_FieldByYear.csv"))

# combine with fields and rankings
wrg_combo <- 
  left_join(wrg_fields, wrg_ranked, by = c("WR_GROUP", "Year")) |> 
  subset(is.finite(diff_rank))

sf_fields_rank <- 
  right_join(sf_fields, wrg_combo, by = "UID") |> 
  select(UID, WR_GROUP, Year, irr_m3_fromPrec, irr_m3_fromWIMAS, irr_m3_diff, diff_rank, geometry)
st_write(sf_fields_rank, file.path("figures+tables", "OpenET-CompareToWIMAS-WRGs_MapDifferences.gpkg"))

sf_fields_rank |> 
  subset(Year == 2016 & diff_rank <= 10) |> 
  st_write(file.path("figures+tables", "OpenET-CompareToWIMAS-WRGs_MapDifferences_2016-best10.gpkg"), delete_layer = T)

sf_fields_rank |> 
  subset(Year == 2016 & diff_rank >= 52) |> 
  st_write(file.path("figures+tables", "OpenET-CompareToWIMAS-WRGs_MapDifferences_2016-worst10.gpkg"), delete_layer = T)


ggplot() +
  geom_sf(data = sf_lema, fill = NA, color = "black") +
  geom_sf(data = sf_fields_rank, aes(fill = diff_rank)) +
  #geom_sf(data = sf_pdiv, color = "red") +
  facet_wrap(~ Year, nrow = 2) +
  scale_fill_viridis_c()


# best 3 WRG: 418, 355, 564
# worst 3 WRG: 115, 262, 726
