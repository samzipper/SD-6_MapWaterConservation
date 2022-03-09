## DataPrep_00_CollectAnnualCDLdata.R
# This script compiles and cleans CDL data extracted from Google Earth Engine.
# GEE script saves data to Google Drive and then I manually downloaded and put
# into 
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
cdlkey$cdlClassName <- as.character(cdlkey$CLASS_NAME)
dataLong2 <- merge(dataLong, cdlkey[,c('VALUE','cdlClassName')],
                   by.x = 'cdlCode', by.y = 'VALUE')

# reorganize
cdlLong <- dataLong2 %>%
  select(c(masterid, Year, status, cdlCode, cdlClassName, area_m2))

write_csv(cdlLong, file.path(dir_data, "GIS", "cdl_tables_lema", "CDL_AnnualYearFieldCropIrrigation.csv"))
