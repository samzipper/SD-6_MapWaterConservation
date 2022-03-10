## DataPrep_00_CollectAnnualCDLdata.R
# This script compiles and cleans CDL data extracted from Google Earth Engine.
# GEE script saves data to Google Drive and then I manually downloaded and put
# into a large file data directory.
#
# Based on script by Jill:
#   https://github.com/jdeines/Deines_etal_2019_LEMA/blob/master/code/data_preparation/00.40_makeMasterDataFile.md

source(file.path("code", "paths+packages.R"))

# directories
dataDir <- file.path(dir_data, "GIS", "cdl_tables_lema")
# cdl key 
cdlkey <- read.csv(file.path(dataDir, 'CDL_key_2014.csv'))

# CDL  annual files -  read in to a single dataframe
cdlfiles <- list.files(dataDir,
                       pattern="*_CDL_area*",
                       full.names = TRUE)

# SCZ: replacing this with a for loop, couldn't figure out some rbind issues
# read in to a single dataframe, removing junk columns and first dummy row
#dataAll <- as.data.frame(do.call(rbind, lapply(cdlfiles, function(x) {
  #csv1 <- read.csv(x)
  #csv1 <- csv1[, -which(names(csv1)%in% c('system.index','.geo','X0','X62',
  #                                        'X251','X252','X253'))]
  #csv1 <- csv1[-1,]    # remove dummy row
  #csv2 <- csv1[csv1$masterid %in% c('sheridan','null_geo9'),]
#}))) 

for (i in 1:length(cdlfiles)){
  csv1 <- read.csv(cdlfiles[i])
  csv1 <- csv1[, -which(names(csv1)%in% c('system.index','.geo','X0','X62',
                                          'X215', 'X228', 'X251','X252','X253'))]
  csv1 <- csv1[-1,]    # remove dummy row
  
  if (i == 1){
    dataAll <- csv1
  } else {
    dataAll <- rbind(dataAll, csv1)
  }
}


# convert to long data format and remove NA's
dataLong <- gather(dataAll, key = cdlCode, value = area_m2, X1:X92)
dataLong <- dataLong[!is.na(dataLong$area_m2),]

# add a crop type column using cdl key
dataLong$cdlCode <- as.numeric(substr(dataLong$cdlCode, start = 2, 
                                      stop = nchar(dataLong$cdlCode)))

# add a derived CropCode column which groups some of the minor CDL categories together
# this will match the crop_names.groups data frame
# see spreadsheet from Jude: clu_zon_maj_cdl2006_2018_legend.xlsx
dataLong$CropCode <- dataLong$cdlCode
dataLong$CropCode[dataLong$cdlCode %in% c(36, 37)] <- 510             # Alfalfa/hay
dataLong$CropCode[dataLong$cdlCode %in% c(152, 176, 59)] <- 520       # Grass/shrub
dataLong$CropCode[dataLong$cdlCode %in% c(141, 142, 143)] <- 530      # Forest
dataLong$CropCode[dataLong$cdlCode %in% c(190, 195)] <- 540           # Wetland
dataLong$CropCode[dataLong$cdlCode %in% c(121, 122, 123, 124)] <- 550 # Developed
dataLong$CropCode[dataLong$cdlCode %in% c(111, 131)] <- 560           # Water/barren
dataLong$CropCode[dataLong$cdlCode %in% c(44, 63)] <- 25              # Other small grains
# check: table(dataLong$CropCode[!(dataLong$CropCode %in% crop_names.groups$CropCode)])

# reorganize
cdlLong <- 
  dataLong %>%
  select(c(masterid, Year, status, CropCode, area_m2)) %>% 
  rename(UID = masterid)

# save overall output
write_csv(cdlLong, file.path(dir_data, "GIS", "cdl_tables_lema", "CDL_AnnualYearFieldCropIrrigationAll.csv"))

# identify dominant crop in each field/year
fieldArea <-
  cdlLong %>% 
  group_by(UID, Year) %>% 
  summarize(area_m2_field = sum(area_m2))

cdlFieldWithTotal <-
  cdlLong %>% 
  group_by(UID, Year, CropCode) %>% 
  summarize(area_m2_crop = sum(area_m2)) %>% 
  left_join(fieldArea, by = c("UID", "Year")) %>% 
  mutate(pctcov = round(area_m2_crop/area_m2_field, 3))

cdlFieldCropMode <-
  cdlFieldWithTotal %>% 
  group_by(UID, Year) %>% 
  filter(pctcov == max(pctcov)) %>% 
  select(-area_m2_crop, -area_m2_field)

# save output
write_csv(cdlFieldCropMode, file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv"))
