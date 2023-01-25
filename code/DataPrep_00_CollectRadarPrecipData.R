## DataPrep_00_CollectRadarPrecipData.R
# This script is intended to collect radar precipitation data for each field
# from rasters created by John Woods.

source(file.path("code", "paths+packages.R"))
library(raster)

# load field boundaries
sf_fields <- st_read(file.path("data", "Fields_NoDups.shp"))
sf_fields_reproj <- st_transform(sf_fields, crs(r_ym))

# load a raster and reproject fields
dir_r <- file.path(dir_data, "radar_precip")
r_crs <- raster(file.path(dir_r, "USA_Ppt_Obs_JAN_2017_b1.tif"))
sf_fields_reproj <- st_transform(sf_fields, crs(r_crs))
sf_fields_reproj <- sf_fields_reproj[1:200, ] # for testing

for (y in 2017:2020){
  # month strings
  mon_str <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  for (m in mon_str){
    # month number
    m_number <- month(mdy(paste0(m, "-01-2020")))
    # load raster
    r_ym <- raster(file.path(dir_r, paste0("USA_Ppt_Obs_", m, "_", y, "_b1.tif")))
    # crop
    r_ym_c <- crop(r_ym, st_transform(sf_fields, crs(r_ym)))
    # disaggregate to deal with small fields
    r_ym_c_d <- disaggregate(r_ym_c, fact=10)
    # extract
    fields_precip_ym <- raster::extract(r_ym_c_d, sf_fields_reproj, fun = mean)
    
    # make data frame
    df_ym <- tibble(Year = y,
                    Month = m_number,
                    UID = sf_fields_reproj$UID,
                    precip_UNITS = fields_precip_ym[,1])
    
    if (y == 2017 & m == mon_str[1]){
      df_out <- df_ym
    } else {
      df_out <- rbind(df_out, df_ym)
    }
  }
  
}


#### stopping here for the day

## maybe need to use this - fill in missing UIDs

length(unique(sf_fields$UID))
length(unique(df_daily$UID))
UID_nodata <- sf_fields$UID[!(sf_fields$UID %in% df_daily$UID)]
sf_fields_data <- sf_fields[!(sf_fields$UID %in% UID_nodata), ]

for (UID in UID_nodata){
  # find closest UID
  UID_dist <- st_distance(sf_fields[sf_fields$UID == UID, ], sf_fields_data)
  UID_closest <- sf_fields_data$UID[which.min(UID_dist)]
  
  # extract met data
  df_daily_closest <- subset(df_daily, UID == UID_closest)
  
  # replace UID
  df_daily_closest$UID <- UID
  
  # add to overall data frame
  df_daily <- bind_rows(df_daily, df_daily_closest)
}

# create water year column
df_daily$WaterYear <- year(df_daily$date_ymd + days(92))

# aggregate to month by field
df_monthly <-
  df_daily |>
  group_by(UID, WaterYear, Year, Month) |>
  summarize(date = max(date_ymd),
            precip_mm = sum(pr),
            ETr_mm = sum(etr),
            ETo_mm = sum(eto))

# aggregate to growing season by field
df_gs <-
  df_monthly |> 
  subset(Month %in% gs_months) |> 
  select(-date, -Month) |> 
  group_by(UID, Year) |> 
  summarize(precip_mm = sum(precip_mm),
            ETr_mm = sum(ETr_mm),
            ETo_mm = sum(ETo_mm))

# aggregate to water year by field
df_wyear <-
  df_monthly |> 
  select(-date, -Month) |> 
  group_by(UID, WaterYear) |> 
  summarize(precip_mm = sum(precip_mm),
            ETr_mm = sum(ETr_mm),
            ETo_mm = sum(ETo_mm))

# aggregate to year by field
df_yearly <-
  df_monthly |>
  group_by(UID, Year) |>
  summarize(precip_mm = sum(precip_mm),
            ETo_mm = sum(ETo_mm),
            ETr_mm = sum(ETr_mm))

# check to make sure all fields have data
if (length(sf_fields$UID[!(sf_fields$UID %in% df_daily$UID)]) > 0) { stop("error - missing fields in daily") }
if (length(sf_fields$UID[!(sf_fields$UID %in% df_monthly$UID)]) > 0) { stop("error - missing fields in monthly") }
if (length(sf_fields$UID[!(sf_fields$UID %in% df_gs$UID)]) > 0) { stop("error - missing fields in gs") }
if (length(sf_fields$UID[!(sf_fields$UID %in% df_wyear$UID)]) > 0) { stop("error - missing fields in gs") }
if (length(sf_fields$UID[!(sf_fields$UID %in% df_yearly$UID)]) > 0) { stop("error - missing fields in yearly") }

# save output
write_csv(df_yearly, file.path("data", "gridmet_AnnualByField.csv"))
write_csv(df_gs, file.path("data", "gridmet_GrowingSeasonByField.csv"))
write_csv(df_gs, file.path("data", "gridmet_WaterYearByField.csv"))
write_csv(df_monthly, file.path(dir_data, "gridMET", "gridmet_MonthlyByField.csv"))

# visualize output
sf_fields |> 
  left_join(subset(df_yearly, Year == 2016), by = "UID") |> 
  ggplot(aes(fill = precip_mm)) + 
  geom_sf()

## compare to data from tom
df_yearly_fromTom <-
  file.path(dir_data, "gridMET", "DataFromTom", "gridmet_AnnualByField.csv") |>
  read_csv()

df_yearly_compare <-
  left_join(df_yearly, df_yearly_fromTom, by = c("Year", "UID"), suffix = c("_GEE", "_Tom"))

ggplot(df_yearly_compare, aes(x = precip_mm_GEE, y = precip_mm_Tom, color = Year)) +
  geom_point()