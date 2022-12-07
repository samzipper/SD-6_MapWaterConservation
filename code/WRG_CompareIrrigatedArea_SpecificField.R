## WRG_CompareIrrigatedArea_SpecificField.R

source(file.path("code", "paths+packages.R"))

# work with raw data from Brownie to make sure understand
dir_WRG <- file.path(dir_data, "data_WRGs_fromBrownie_20221006", "extracted_layers")

# WR_GROUP to focus on
WRG <- 230

## load fields for each WR_GROUP
# fields in WRG
df_fields <- 
  read_csv(file.path(dir_WRG, "AIM_wimasPU_intersection_table.csv")) |> 
  subset(WR_GROUP %in% WRG) |> 
  subset(OVLP_PCT > 0.5)
UIDs <- unique(df_fields$UID)

# irrigation status
df_irr <- read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  subset(Year %in% seq(2016, 2021) &
           UID %in% UIDs)

# field area
df_att <- read_csv(file.path("data", "Fields_Attributes-Spatial.csv")) |> 
  subset(UID %in% UIDs) |> 
  mutate(area_ac = area_m2/4046.86)

df_irr.att <- left_join(df_irr, df_att, by = "UID")

# calculate annual irrigated area
df_irr.att |> 
  subset(IrrigatedPrc > 0.5) |> 
  group_by(Year) |> 
  summarize(irrAcres = sum(area_ac),
            n_fields = n())

# load WIMAS data
sf_wrg <- 
  sf::st_read(file.path(dir_WRG, "wimas_pdfile.gpkg")) |> 
  subset(Wr_group == WRG)
PDIVs <- unique(sf_wrg$PDIV_ID)

df_use <- read_csv(file.path(dir_WRG, "water_use_1990_2021_table.csv")) |> 
  subset(PDIV_ID %in% PDIVs &
           WUA_YEAR %in% seq(2016, 2021))



