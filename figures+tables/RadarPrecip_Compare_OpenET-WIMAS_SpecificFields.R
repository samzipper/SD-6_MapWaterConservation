# RadarPrecip_Compare_OpenET-WIMAS_SpecificFields.R

source(file.path("code", "paths+packages.R"))

## load data
# load pairs
pdiv.uid_pairs <- read_csv(file.path("data", "WRGs_PDIV-UID_ManualPairs.csv"))

# load fields and WR groups for each UID
wrg.uid <- 
  read_csv(file.path("data", "WRGs_WRGbyField.csv"))

# determine if any of these fields are in WRGs with other fields
wrgs_in_pairs <- subset(wrg.uid, UID %in% pdiv.uid_pairs$UID)$WR_GROUP
wrg.uid |> 
  subset(WR_GROUP %in% wrgs_in_pairs) |> 
  group_by(WR_GROUP) |> 
  summarize(n_fields = n())  # all of them have multiple fields

# get field-based irrigation estimates, just for the field identified in pair
ts <- "GrowingSeason"
uid.irr <- 
  read_csv(file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, "_RadarPrecip.csv"))) |> 
  subset(UID %in% wrg.uid$UID) |> 
  subset(Irrigation)

# pdiv data
dir_WRG <- file.path(dir_data, "data_WRGs_fromBrownie_20221216", "extracted_layers")
wrg.pdiv <- 
  file.path(dir_WRG, "wimas_pdfile_wrg_10mile_sd6.gpkg") |> 
  st_read() |> 
  subset(PDIV_ID %in% pdiv.uid_pairs$PDIV_ID)

unique(wrg.pdiv$Grp_pd_cnt) #check: should be only 1

# load wrg water use for each (WRG were selected for only 1 PDIV, so this will be same as PDIV water use) 
wrg.wuse <- 
  read_csv(file.path("data", "WRGs_UseByWRG.csv")) |> 
  subset(WR_GROUP %in% wrg.uid$WR_GROUP) |> 
  dplyr::select(WR_GROUP, Year, WRGirrigationTotal_m3, WRGirrAreaReported_m2)

# load area of fields
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv")) |> 
  subset(UID %in% pdiv.uid_pairs$UID) |> 
  dplyr::select(UID, area_m2)

## combine
# start with fields/algorithms/years: extract irrigation volumes for each field
df_all <-
  subset(uid.irr, UID %in% pdiv.uid_pairs$UID) |> 
  # add field area by UID
  left_join(fields_spatial, by = "UID") |> 
  dplyr::select(-within_lema, -within_buffer) |> 
  # add PDIV_ID based on UID from pairs
  left_join(pdiv.uid_pairs, by = "UID") |> 
  # add WR_GROUP based on UID from pairs
  left_join(wrg.uid, by = "UID") |> 
  # add water use from WR_GROUP
  left_join(wrg.wuse, by = c("Year", "WR_GROUP"))

## plot
# compare reported irrigated area to field
ggplot(subset(df_all, Algorithm == "ensemble"), aes(x = WRGirrAreaReported_m2, y = area_m2)) +
  geom_abline(intercept = 0, color = col.gray) +
  geom_point() +
  scale_x_continuous(name = "Reported irrigated area [m2]") +
  scale_y_continuous(name = "Field irrigated area [m2]")

# compare reported irrigation and estimated irrigation
ggplot(df_all, aes(x = 1000*WRGirrigationTotal_m3/WRGirrAreaReported_m2, 
                   y = FieldIrrigation_mm, color = Algorithm)) +
  geom_abline(intercept = 0, color = col.gray) +
  geom_point() +
  facet_wrap(~Year) +
  scale_x_continuous(name = "Reported irrigation [m3]") +
  scale_y_continuous(name = "Estimated irrigation [mm]") +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms)

######## some manual investigations
pdiv <- pdiv.uid_pairs$PDIV_ID[5]
wrg <- wrg.pdiv$Wr_group[wrg.pdiv$PDIV_ID == pdiv]
uid.main <- pdiv.uid_pairs$UID[pdiv.uid_pairs$PDIV_ID == pdiv]
uid.more <- wrg.uid$UID[wrg.uid$WR_GROUP == wrg]

# pdiv number
pdiv

# is in wr_group 
wrg

# with the target field
uid.main

# and the other fields
uid.more[!(uid.more == uid.main)]

# for PDIV 
#  1779: 1 additional field (corners of the section)
#  27831: 4 additional fields (corners of the section)
#  20584: 6 additional fields (corners of section + a wedge in middle of field)
#  16466: 1 additional field (corners of section)
#  8142: 6 additional fields (corners of section + next section over)
