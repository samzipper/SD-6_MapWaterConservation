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

## read in the files that link field UIDs to WIMAS water rights group and summarize water use
wrg_fields <- 
  file.path(dir_WRG, "AIM_wimasPU_intersection_table.csv") |> 
  read_csv() |> 
  dplyr::select(-OBJECTID)

wrg_usebyyear <-
  file.path(dir_WRG, "water_use_qty_group_2016_2021.csv") |> 
  read_csv() |> 
  dplyr::select(-OBJECTID, -starts_with("Auth"), -avg_irr_2016_2020)

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

## save output:
# WRgroups_FieldByYear.csv = CSV file with group for each UID for each year
readr::write_csv(wrg_fields, file.path("data", "WRgroups_WRGbyField.csv"))

# WRgroups_UseByWRG.csv = CSV file with water use by group
wrg_summary |> 
  dplyr::mutate(WaterUse_m3 = Wuse_af*1233.48185532,
                Irrigation_m3 = Irr_af*1233.48185532,
                IrrArea_m2 = Irr_Acres*4046.8564) |> 
  dplyr::select(WR_GROUP, Year, WaterUse_m3, Irrigation_m3, IrrArea_m2) |> 
  dplyr::arrange(Year, WR_GROUP) |> 
  readr::write_csv(file.path("data", "WRgroups_UseByWRG.csv"))
