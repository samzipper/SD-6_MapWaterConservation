## DataPrep_02_WRgroups_SummarizeUse+Fields.R
# This aggregated data on the basis of 'water rights groupings', which are combinations of
# water rights, use(s) made of water, point(s)  of diversion, authorized quantity and rates from WIMAS.
# 
# This will make two output files: 
#  - WRgroups_FieldByYear.csv = CSV file with group for each UID for each year
#  - WRgroups_UseByWRG.csv = CSV file with water use by group

source(file.path("code", "paths+packages.R"))

## read in shapefile that defines water rights groups
# Description of files from Brownie (email, Dec 3 2019):
#   These are all the water rights and points of diversion smashed together into a single file. 
#   They have been grouped together based on their overlapping point(s) of diversion and place(s)
#   of use.  The last set of fields lists the a unique number for a water right grouping (Wr_group) 
#   and how many water rights (Grp_wr_cnt) and points of diversion (Grp_pd_cnt) make up the group.
#### NOT NEEDED I DON'T THINK
sf_wrg <- sf::st_read(file.path(dir_GIS, "WaterRightsGroupTables", "wimas_pdfile_sd6.shp")) %>% 
  dplyr::select(-OBJECTID)

## read in the DBF files that link field UIDs to WIMAS water rights group
# Description of files from Brownie (email, Dec 3 2019):
#   Tables by year that lists the overlay results between the CLU and WIMAS 40 acre tract
#   layers.  The primary keys here are the UID and WR_GROUP fields.  If a UID duplicates, 
#   there is more than one 40-acre intersecting it.  In those cases, I would take who ever 
#   has the highest overlap percentage (OVLP_PCT field) and then delete the other.
#
# DBF files contain the following fields:
#  - OBJECTID = row number, can be ignored
#  - UID = ID for field
#  - WR_GROUP = water rights group
#  - GRP_ACRES = total acres covered by fields in WR group
#  - TRGT_ACRES = ??acres of that UID??
#  - OVLP_ACRES = acres of that UID that overlaps with the water rights group
#  - OVLP_PCT = percent of that UID that overlaps with the water rights group
yr_range <- seq(2006, 2018)
for (yr in yr_range){
  wrg_fields_yr <- 
    foreign::read.dbf(file.path(dir_GIS, "WaterRightsGroupTables", paste0("clu_irr_wimas_puse_", yr, ".dbf"))) %>% 
    dplyr::select(-OBJECTID) %>% 
    # eliminate duplicated UIDs following Brownie's suggestion (highest OVLP_PCT)
    dplyr::group_by(UID) %>% 
    dplyr::filter(OVLP_PCT == max(OVLP_PCT)) %>% 
    # which(duplicated(wrg_fields_yr_trimmed$UID)) # there 2 UIDs that remain in 2 groups - these are very small (< 3 acres), just choose one
    subset(!duplicated(UID)) %>% 
    dplyr::mutate(Year = yr) %>% 
    dplyr::arrange(WR_GROUP, Year)
  
  if (yr == yr_range[1]){
    wrg_fields <- wrg_fields_yr
  } else {
    wrg_fields <- dplyr::bind_rows(wrg_fields, wrg_fields_yr)
  }
}

## read in table summarizing each water rights group
# Description of files from Brownie (email, Dec 3 2019):
#  A table that lists for each water right group, its total quantity and
#  reported water use and acres irrigated from 2006 to 2018.
#   - Wuse_YYYY = total water use in that year [acre-feet]
#   - Irr_YYYY = irrigation water use in that year [acre-feet]
#   - Acres_YYYY = irrigated acreage in that year [acres]
wrg_dbf <- 
  foreign::read.dbf(file.path(dir_GIS, "WaterRightsGroupTables", "wimas_group_summaries.dbf")) %>% 
  dplyr::select(-OBJECTID)

for (yr in yr_range){
  wrg_yr <- wrg_dbf[,c("WR_GROUP", paste0("Wuse_", yr), paste0("Irr_", yr), paste0("Acres_", yr))]
  colnames(wrg_yr) <- c("WR_GROUP", "Wuse_af", "Irr_af", "Irr_Acres")
  wrg_yr$Year <- yr
  
  if (yr == yr_range[1]){
    wrg_summary <- wrg_yr
  } else {
    wrg_summary <- dplyr::bind_rows(wrg_summary, wrg_yr)
  }
}

## save output:
# WRgroups_FieldByYear.csv = CSV file with group for each UID for each year
readr::write_csv(wrg_fields, file.path("data", "WRgroups_FieldByYear.csv"))

# WRgroups_UseByWRG.csv = CSV file with water use by group
wrg_summary %>% 
  dplyr::mutate(WaterUse_m3 = Wuse_af*1233.48185532,
                Irrigation_m3 = Irr_af*1233.48185532,
                IrrArea_m2 = Irr_Acres*4046.8564) %>% 
  dplyr::select(WR_GROUP, Year, WaterUse_m3, Irrigation_m3, IrrArea_m2) %>% 
  dplyr::arrange(Year, WR_GROUP) %>% 
  readr::write_csv(file.path("data", "WRgroups_UseByWRG.csv"))
