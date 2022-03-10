## DataPrep_00_CollectAnnualAIMdata.R
# This script compiles and cleans AIM data extracted from Google Earth Engine.
# GEE script saves data to Google Drive and then I manually downloaded and put
# into a large file data directory.
#
# Based on script by Jill:
#   https://github.com/jdeines/Deines_etal_2019_LEMA/blob/master/code/data_preparation/00.40_makeMasterDataFile.md

source(file.path("code", "paths+packages.R"))

# directories
dataDir <- file.path(dir_data, "GIS", "AIM")

csv1 <- read_csv(file.path(dataDir,'1984-2020_AIM-HPA_Deines_etal_RSE_v01_extend_1984-2020_samsPolygons.csv'))

# some cleaning
csv1$`1`[is.na(csv1$`1`)] <- 0
csv1$`0`[is.na(csv1$`0`)] <- 0
csv1$area_total <- csv1$`1` + csv1$`0`

csv1$IrrigatedPrc <- round(csv1$`1`/csv1$area_total, 3)

csv1 %>% 
  dplyr::select(UID, year, IrrigatedPrc) %>% 
  rename(Year = year) %>% 
  write_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv"))
