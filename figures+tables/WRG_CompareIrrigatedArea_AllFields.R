## WRG_CompareIrrigatedArea_SpecificField.R

source(file.path("code", "paths+packages.R"))

# work with raw data from Brownie to make sure understand
dir_WRG <- file.path(dir_data, "data_WRGs_fromBrownie_20221006", "extracted_layers")

## load fields for each WR_GROUP
# fields in WRG
df_fields <- 
  read_csv(file.path(dir_WRG, "AIM_wimasPU_intersection_table.csv")) |> 
  subset(OVLP_PCT > 0.5)

# irrigation status
df_irr <- read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  subset(Year %in% seq(2016, 2021))

# field area
df_att <- read_csv(file.path("data", "Fields_Attributes-Spatial.csv")) |> 
  mutate(area_ac = area_m2/4046.86)

df_irr.att <- left_join(df_irr, df_att, by = "UID")
df_irr.att.wrg <- left_join(df_irr.att, df_fields, by = "UID")

# for each WR_GROUP...
#  drop nonirrigated fields
#  drop fields not in WR_GROUP
#  sum acres by year and WRG
df_wrg_acres <- 
  df_irr.att.wrg |> 
  subset(IrrigatedPrc >= 0.5 & is.finite(WR_GROUP)) |> 
  group_by(Year, WR_GROUP) |> 
  summarize(IrrAcres_WRG.AIM = sum(OVLP_ACRES))

## load WIMAS reported irrigated acreage
df_use <- read_csv(file.path(dir_WRG, "water_use_qty_group_2016_2021.csv")) |> 
  select(WR_GROUP, starts_with("Acres_")) |> 
  pivot_longer(-WR_GROUP, values_to = "IrrAcres_WIMAS")
df_use$Year <- as.numeric(str_sub(df_use$name, start = 7, end = 10))

## join and compare
df_both <- left_join(df_wrg_acres, df_use, by = c("WR_GROUP", "Year")) 
max(c(df_both$IrrAcres_WRG.AIM, df_both$IrrAcres_WIMAS), na.rm = T)

ggplot(df_both, aes(x = IrrAcres_WRG.AIM, y = IrrAcres_WIMAS)) +
  geom_point(shape = 1) +
  facet_wrap(~Year) +
  scale_x_continuous(name = "Mapped Irrigated Acres from WRG Fields", 
                     breaks = seq(0, 1000, 250), limits = c(0, 1200)) +
  scale_y_continuous(name = "Reported Irrigated Acres from WIMAS", 
                     breaks = seq(0, 1000, 250), limits = c(0, 1200)) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  geom_abline(intercept = 0, slope = 1.1, color = col.cat.red, linetype = "dashed") +
  geom_abline(intercept = 0, slope = 0.9, color = col.cat.red, linetype = "dashed") +
  coord_equal()

ggsave(file.path("figures+tables", "WRG_CompareIrrigatedArea_AllFields.png"),
       width = 190, height = 120, units = "mm")
