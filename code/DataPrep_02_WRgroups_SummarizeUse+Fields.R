## DataPrep_02_WRgroups_SummarizeUse+Fields.R
# This aggregated data on the basis of 'water rights groupings', which are combinations of
# water rights, use(s) made of water, point(s)  of diversion, authorized quantity and rates from WIMAS.
# 
# This will make two output files: 
#  - WRgroups_FieldByYear.csv = CSV file with group for each UID for each year
#  - WRgroups_UseByWRG.csv = CSV file with water use by group

source(file.path("code", "paths+packages.R"))

dir_WRG <- file.path(dir_data, "data_WRGs_fromBrownie_20221216", "extracted_layers")

## load data
# field UIDs for each WR_GROUP
wrg_fields <- 
  file.path(dir_WRG, "AIM_wimasPU_intersection_table.csv") |> 
  read_csv() |> 
  dplyr::select(-OBJECTID)

# pdiv for each WR_GROUP
wrg_pdiv <- 
  file.path(dir_WRG, "wimas_pdfile_wrg_10mile_sd6.gpkg") |> 
  st_read()

# WRG use by year
wrg_usebyyear <-
  file.path(dir_WRG, "water_use_qty_group_2016_2021.csv") |> 
  read_csv() |> 
  dplyr::select(-OBJECTID, -starts_with("Auth"))

# field attributes
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5)

## QA/QC section - manual inspection
# any WRG with no water use information?
WRGs_nodata <- wrg_usebyyear$WR_GROUP[is.na(wrg_usebyyear$Wuse_2018)]
length(WRGs_nodata) # 340
wrg_usebyyear_trim <- subset(wrg_usebyyear, !(WR_GROUP %in% WRGs_nodata))

# any WRG in usebyyear but not fields? (reported water use, but no fields)
sum(!(wrg_usebyyear$WR_GROUP %in% wrg_fields$WR_GROUP)) # including all WR_GROUPS: 355 out of 1018 missing
sum(!(wrg_usebyyear_trim$WR_GROUP %in% wrg_fields$WR_GROUP)) # screening blank WR_GROUPS: 38 out of 678

# any irrigated fields that are not in a WR_GROUP?
UID_wrg <- unique(wrg_fields$UID)
UID_LEMA <- subset(fields_spatial, within_lema)$UID
UID_notLEMA <- subset(fields_spatial, !within_lema)$UID
UID_irr <- unique(subset(fields_irrigation, Irrigation)$UID)
UID_irr_LEMA <- UID_LEMA[UID_LEMA %in% UID_irr]
UID_irr_notLEMA <- UID_irr[!(UID_irr %in% UID_irr_LEMA)]

sum(!(UID_irr %in% UID_wrg))      # 7 irrigated fields not in WR_GROUP
sum(!(UID_irr_LEMA %in% UID_wrg)) # none are in LEMA

# any WRG with no water use information that has irrigated fields?
UID_noWRGdata <- wrg_fields$UID[wrg_fields$WR_GROUP %in% WRGs_nodata]  # 171 have associated fields
UID_noWRGdata_LEMA <- UID_noWRGdata[UID_noWRGdata %in% UID_irr_LEMA]   # 7 fields that are in LEMA...
unique(wrg_fields$WR_GROUP[wrg_fields$UID %in% UID_noWRGdata_LEMA])    # ... from 3 WR_GROUPS
fields_irrigation |> 
  subset(UID %in% UID_noWRGdata_LEMA &
           Year >= 2016) |> 
  arrange(UID)

## clean up WRG-UID links by removing small OVLP_PCT
ggplot(wrg_fields, aes(x = OVLP_PCT)) +
  geom_histogram(breaks = seq(0, 1, 0.05))

OVLP_cutoff <- 0.25

wrg_fields_trim <- subset(wrg_fields, OVLP_PCT > OVLP_cutoff)

## summarize/tidy water use by WRG each year

# identify WRGs that have fields in LEMA and outside LEMA
WRGs_LEMA <- unique(subset(wrg_fields_trim, UID %in% UID_LEMA)$WR_GROUP)
WRGs_notLEMA <- unique(subset(wrg_fields_trim, UID %in% UID_notLEMA)$WR_GROUP)

# 9 WRGs are in both LEMA and notLEMA
WRGs_LEMA[WRGs_LEMA %in% WRGs_notLEMA]

# for each WRG, calculate fraction of irrigation area inside LEMA
wrg_all <- unique(wrg_fields_trim$WR_GROUP)
yr_range <- seq(2016, 2021)

for (yr in yr_range){
  df_wrg_LEMAfrac_y <- tibble(Year = yr,
                              WR_GROUP = wrg_all,
                              LEMA_fieldArea_m2 = NaN,
                              notLEMA_fieldArea_m2 = NaN,
                              LEMA_irrFieldArea_m2 = NaN,
                              notLEMA_irrFieldArea_m2 = NaN)
  
  fields_irrigation_y <- subset(fields_irrigation, Year == yr)
  
  for (i in 1:length(wrg_all)){
    w <- df_wrg_LEMAfrac_y$WR_GROUP[i]
    
    # get fields in WRG
    w_UIDs <- unique(subset(wrg_fields_trim, WR_GROUP == w)$UID)
    w_UIDs_LEMA <- w_UIDs[w_UIDs %in% UID_LEMA]
    w_UIDs_notLEMA <- w_UIDs[w_UIDs %in% UID_notLEMA]
    
    # identify irrigated and non-irrigated fields
    w_UIDs_irr_LEMA <- w_UIDs_LEMA[w_UIDs_LEMA %in% subset(fields_irrigation_y, Irrigation)$UID]
    w_UIDs_irr_notLEMA <- w_UIDs_notLEMA[w_UIDs_notLEMA %in% subset(fields_irrigation_y, Irrigation)$UID]
    
    # get irrigated percent of pixels for each irrigated field
    w_UIDs_irrPrc_LEMA <- fields_irrigation_y$IrrigatedPrc[match(w_UIDs_irr_LEMA, fields_irrigation_y$UID)]
    w_UIDs_irrPrc_notLEMA <- fields_irrigation_y$IrrigatedPrc[match(w_UIDs_irr_notLEMA, fields_irrigation_y$UID)]
    
    # get area for each irrigated field
    w_UIDs_area_LEMA <- fields_spatial$area_m2[match(w_UIDs_irr_LEMA, fields_spatial$UID)]
    w_UIDs_area_notLEMA <- fields_spatial$area_m2[match(w_UIDs_irr_notLEMA, fields_spatial$UID)]
    
    df_wrg_LEMAfrac_y$LEMA_fieldArea_m2[i] <-
      sum(fields_spatial$area_m2[fields_spatial$UID %in% w_UIDs_LEMA])
    df_wrg_LEMAfrac_y$notLEMA_fieldArea_m2[i] <-
      sum(fields_spatial$area_m2[fields_spatial$UID %in% w_UIDs_notLEMA])
    
    df_wrg_LEMAfrac_y$LEMA_irrFieldArea_m2[i] <-
      sum(w_UIDs_area_LEMA*w_UIDs_irrPrc_LEMA)
    df_wrg_LEMAfrac_y$notLEMA_irrFieldArea_m2[i] <-
      sum(w_UIDs_area_notLEMA*w_UIDs_irrPrc_notLEMA)
  }
  
  if (yr == yr_range[1]){
    df_wrg_LEMAfrac <- df_wrg_LEMAfrac_y
  } else {
    df_wrg_LEMAfrac <- bind_rows(df_wrg_LEMAfrac, df_wrg_LEMAfrac_y)
  }
}

# calculate field area and irrigated field area fraction
df_wrg_LEMAfrac$LEMA_fieldArea_fraction <- 
  df_wrg_LEMAfrac$LEMA_fieldArea_m2/(df_wrg_LEMAfrac$LEMA_fieldArea_m2 + df_wrg_LEMAfrac$notLEMA_fieldArea_m2)
df_wrg_LEMAfrac$LEMA_irrFieldArea_fraction <- 
  df_wrg_LEMAfrac$LEMA_irrFieldArea_m2/(df_wrg_LEMAfrac$LEMA_irrFieldArea_m2 + df_wrg_LEMAfrac$notLEMA_irrFieldArea_m2)

# get rid of WRGs that don't have fields
df_wrg_LEMAfrac <- subset(df_wrg_LEMAfrac, is.finite(LEMA_fieldArea_fraction))

# check number of split WRGs across LEMA/notLEMA border
df_wrg_LEMAfrac$WR_GROUP[df_wrg_LEMAfrac$LEMA_fieldArea_fraction >= 0.01 & df_wrg_LEMAfrac$LEMA_fieldArea_fraction <= 0.99]

# calculate number of WRGs inside and outside lema
sum(df_wrg_LEMAfrac$LEMA_fieldArea_fraction > 0.01)  # total WRGs including LEMA
sum(df_wrg_LEMAfrac$LEMA_fieldArea_fraction > 0.01 & df_wrg_LEMAfrac$LEMA_fieldArea_fraction < 1) # total WRGs with land inside and outside LEMA

# sum total and irrigation water use for each WRG in each year
for (yr in yr_range){
  wrg_yr <- wrg_usebyyear_trim[,c("WR_GROUP", paste0("Wuse_", yr), paste0("Irr_", yr), paste0("Acres_", yr))]
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
  left_join(df_wrg_LEMAfrac, by = c("Year", "WR_GROUP")) |> 
  mutate(WRGirrigationTotal_m3 = Irr_af*1233.48185532,
         WRGirrigationLEMA_m3 = WRGirrigationTotal_m3*LEMA_irrFieldArea_fraction,
         WRGirrAreaReported_m2 = Irr_Acres*4046.8564) |> 
  select(-Wuse_af, -Irr_af, -Irr_Acres)

## calculate total annual LEMA-scale irrigation for each water rights group
wrg_irrigation_LEMA <-
  wrg_summary_out |> 
  group_by(Year) |> 
  summarize(WIMASirrigationLEMA_m3 = sum(WRGirrigationLEMA_m3, na.rm = T))

## save output:
# WRGs_FieldByYear.csv = CSV file with group for each UID for each year
#  screen fields with minimal OVLP_PCT
write_csv(wrg_fields_trim, file.path("data", "WRGs_WRGbyField.csv"))

# WRGs_UseByWRG.csv = CSV file with water use by group
write_csv(wrg_summary_out, file.path("data", "WRGs_UseByWRG.csv"))

# WRGs_PDIVbyWRG.gpkg = geopackage file with PDIV for each WR_GROUP
wrg_pdiv |> 
  dplyr::select(PDIV_ID, Wr_group, Grp_wr_cnt, Grp_pd_cnt, UMW_CODE) |> 
  rename(WR_GROUP = Wr_group,
         WRG_nWaterRight = Grp_wr_cnt,
         WRG_nPDIV = Grp_pd_cnt) |> 
  st_write(file.path("data", "WRGs_PDIVbyWRG.gpkg"), append = F)

# WRGs_LEMAtotalIrrigation.csv = CSV file with total LEMA reported water use by year
write_csv(wrg_irrigation_LEMA, file.path("data", "WRGs_LEMAtotalIrrigation.csv"))
