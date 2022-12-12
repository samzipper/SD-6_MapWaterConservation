## DataPrep_02_WRgroups_SummarizeUse+Fields.R
# This aggregated data on the basis of 'water rights groupings', which are combinations of
# water rights, use(s) made of water, point(s)  of diversion, authorized quantity and rates from WIMAS.
# 
# This will make two output files: 
#  - WRgroups_FieldByYear.csv = CSV file with group for each UID for each year
#  - WRgroups_UseByWRG.csv = CSV file with water use by group

source(file.path("code", "paths+packages.R"))

dir_WRG <- file.path(dir_data, "data_WRGs_fromBrownie_20221006", "extracted_layers")

## read in shapefile that defines water rights groups
# Description of files from Brownie (email, Dec 3 2019):
#   These are all the water rights and points of diversion smashed together into a single file. 
#   They have been grouped together based on their overlapping point(s) of diversion and place(s)
#   of use.  The last set of fields lists the a unique number for a water right grouping (Wr_group) 
#   and how many water rights (Grp_wr_cnt) and points of diversion (Grp_pd_cnt) make up the group.
#### NOT NEEDED I DON'T THINK
sf_wrg <- sf::st_read(file.path(dir_WRG, "wimas_pdfile.gpkg")) |> 
  dplyr::select(-OBJECTID)

## link field UIDs to WIMAS water rights group
wrg_fields <- 
  file.path(dir_WRG, "AIM_wimasPU_intersection_table.csv") |> 
  read_csv() |> 
  dplyr::select(-OBJECTID)

# clean up WRG-UID links by removing small OVLP_PCT
ggplot(wrg_fields, aes(x = OVLP_PCT)) +
  geom_histogram(breaks = seq(0, 1, 0.05))

OVLP_cutoff <- 0.25

wrg_fields_trim <- subset(wrg_fields, OVLP_PCT > OVLP_cutoff)

## summarize/tidy water use by WRG each year
# load WRG use by year
wrg_usebyyear <-
  file.path(dir_WRG, "water_use_qty_group_2016_2021.csv") |> 
  read_csv() |> 
  dplyr::select(-OBJECTID, -starts_with("Auth"), -avg_irr_2016_2020)

# load field attributes
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5)

## calculate fraction of WRG field irrigated area that is in LEMA vs outside LEMA
# list of UIDs for irrigated fields in/out of LEMA
UID_LEMA <- subset(fields_spatial, within_lema)$UID
UID_irr <- unique(subset(fields_irrigation, Irrigation)$UID)
UID_irr_LEMA <- UID_LEMA[UID_LEMA %in% UID_irr]
UID_irr_notLEMA <- UID_irr[!(UID_irr %in% UID_irr_LEMA)]

# identify WRGs that have irrigated fields in LEMA and outside LEMA
WRGs_LEMA <- unique(subset(wrg_fields_trim, UID %in% UID_irr_LEMA)$WR_GROUP)
WRGs_notLEMA <- unique(subset(wrg_fields_trim, UID %in% UID_irr_notLEMA)$WR_GROUP)

# for each WRG, calculate fraction of irrigation area inside LEMA
wrg_all <- unique(wrg_fields_trim$WR_GROUP)
df_wrg_LEMAfrac <- tibble(WR_GROUP = WRGs_all,
                          LEMA_irrFieldArea_m2 = NaN,
                          notLEMA_irrFieldArea_m2 = NaN)

for (i in 1:length(wrg_all)){
  w <- df_wrg_LEMAfrac$WR_GROUP[i]
  
  # get area of fields in LEMA
  w_UIDs <- unique(subset(wrg_fields_trim, WR_GROUP == w)$UID)
  w_UIDs_LEMA <- w_UIDs[w_UIDs %in% UID_irr_LEMA]
  w_UIDs_notLEMA <- w_UIDs[w_UIDs %in% UID_irr_notLEMA]
  
  df_wrg_LEMAfrac$LEMA_irrFieldArea_m2[i] <-
    sum(fields_spatial$area_m2[fields_spatial$UID %in% w_UIDs_LEMA])
  df_wrg_LEMAfrac$notLEMA_irrFieldArea_m2[i] <-
    sum(fields_spatial$area_m2[fields_spatial$UID %in% w_UIDs_notLEMA])
}

# calculate irrigated fraction
df_wrg_LEMAfrac$LEMA_irrFieldArea_fraction <- 
  df_wrg_LEMAfrac$LEMA_irrFieldArea_m2/(df_wrg_LEMAfrac$LEMA_irrFieldArea_m2 + df_wrg_LEMAfrac$notLEMA_irrFieldArea_m2)

# get rid of WRGs that don't have irrigated fields
df_wrg_LEMAfrac <- subset(df_wrg_LEMAfrac, is.finite(LEMA_irrFieldArea_fraction))

# find number of split WRGs across LEMA/notLEMA border
df_wrg_LEMAfrac$WR_GROUP[df_wrg_LEMAfrac$LEMA_irrFieldArea_fraction >= 0.01 & df_wrg_LEMAfrac$LEMA_irrFieldArea_fraction <= 0.99]

# sum total and irrigation water use for each WRG in each year
yr_range <- seq(2016, 2021)
for (yr in yr_range){
  wrg_yr <- wrg_usebyyear[,c("WR_GROUP", paste0("Wuse_", yr), paste0("Irr_", yr), paste0("Acres_", yr))]
  colnames(wrg_yr) <- c("WR_GROUP", "Wuse_af", "Irr_af", "Irr_Acres")
  wrg_yr$Year <- yr
  
  if (yr == yr_range[1]){
    wrg_summary <- wrg_yr
  } else {
    wrg_summary <- dplyr::bind_rows(wrg_summary, wrg_yr)
  }
}

# add in LEMA fraction
wrg_summary_out <- 
  wrg_summary |> 
  arrange(Year, WR_GROUP) |> 
  left_join(df_wrg_LEMAfrac, by = "WR_GROUP") |> 
  mutate(WRGirrigationTotal_m3 = Irr_af*1233.48185532,
         WRGirrigationLEMA_m3 = WRGirrigationTotal_m3*LEMA_irrFieldArea_fraction,
         WRGirrAreaReported_m2 = Irr_Acres*4046.8564) |> 
  select(-Wuse_af, -Irr_af, -Irr_Acres)
  
# inspect NA values - ask Brownie about this
wrg_lema <- df_wrg_LEMAfrac$WR_GROUP[df_wrg_LEMAfrac$LEMA_irrFieldArea_fraction > 0]
wrg_usebyyear$WR_GROUP[(is.na(wrg_usebyyear$Wuse_2020))] %in% wrg_all
wrg_usebyyear$WR_GROUP[(is.na(wrg_usebyyear$Wuse_2020))] %in% wrg_lema
 # WR_GROUP 9355 is NA and in LEMA
fields_irrigation[fields_irrigation$UID %in% wrg_fields_trim$UID[wrg_fields_trim$WR_GROUP == 9355], ] |> 
  subset(Year >= 2016) |> 
  arrange(UID, Year) |> 
  print(n = 50)

# inspect WRGs that have water use by are not in intersection table - ask Brownie about this
sum(!(wrg_usebyyear$WR_GROUP %in% wrg_fields$WR_GROUP)) # some in water_use_qty_group_2016_2021.csv are missing from AIM_wimasPU_intersection_table.csv
sum(!(wrg_fields$WR_GROUP %in% wrg_usebyyear$WR_GROUP)) # everything in AIM_wimasPU_intersection_table.csv is in water_use_qty_group_2016_2021.csv

## calculate total annual LEMA-scale irrigation for each water rights group
wrg_irrigation_LEMA <-
  wrg_summary_out |> 
  group_by(Year) |> 
  summarize(WIMASirrigationLEMA_m3 = sum(WRGirrigationLEMA_m3, na.rm = T))

## save output:
# WRgroups_FieldByYear.csv = CSV file with group for each UID for each year
#  screen fields with minimal OVLP_PCT
write_csv(wrg_fields_trim, file.path("data", "WRGs_WRGbyField.csv"))

# WRgroups_UseByWRG.csv = CSV file with water use by group
write_csv(wrg_summary_out, file.path("data", "WRGs_UseByWRG.csv"))

# WRgroups_LEMAtotalIrrigation.csv = CSV file with total LEMA reported water use by year
write_csv(wrg_irrigation_LEMA, file.path("data", "WRGs_LEMAtotalIrrigation.csv"))
