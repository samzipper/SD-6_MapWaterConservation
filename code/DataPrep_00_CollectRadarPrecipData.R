## DataPrep_00_CollectRadarPrecipData.R
# This script is intended to collect radar precipitation data for each field
# from rasters created by John Woods.

source(file.path("code", "paths+packages.R"))
library(raster)

# load field boundaries
sf_fields <- st_read(file.path("data", "Fields_NoDups.shp"))

# load a raster and reproject fields
dir_r <- file.path(dir_data, "radar_precip")
r_crs <- raster(file.path(dir_r, "USA_Ppt_Obs_JAN_2017_b1.tif"))
sf_fields_reproj <- st_transform(sf_fields, crs(r_crs))
#sf_fields_reproj <- sf_fields_reproj[1:500, ] # trim for testing

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
    fields_precip_ym <- raster::extract(r_ym_c_d, sf_fields_reproj, fun = mean) # units are inches/month
    
    # make data frame
    df_ym <- tibble(Year = y,
                    Month = m_number,
                    UID = sf_fields_reproj$UID,
                    precip_mm = fields_precip_ym[,1]*25.4)
    
    if (y == 2017 & m == mon_str[1]){
      df_monthly <- df_ym
    } else {
      df_monthly <- rbind(df_monthly, df_ym)
    }
    
    print(paste0(y, " ", m, " complete, ", Sys.time()))
    
  }
  
  
}

## fill in missing data
UID_nodata <- unique(df_monthly$UID[is.na(df_monthly$precip_mm)])
sf_fields_data <- sf_fields_reproj[!(sf_fields_reproj$UID %in% UID_nodata), ]

for (U in UID_nodata){
  # find closest UID
  UID_dist <- st_distance(sf_fields_reproj[sf_fields_reproj$UID == U, ], sf_fields_data)
  UID_closest <- sf_fields_data$UID[which.min(UID_dist)]
  
  # extract met data
  df_monthly_closest <- subset(df_monthly, UID == UID_closest)
  
  # replace UID
  df_monthly_closest$UID <- U
  
  # add to overall data frame
  df_monthly <- 
    df_monthly |> 
    subset(UID != U) |> 
    bind_rows(df_monthly_closest)
}

# check for any missing data
UID_nodata <- unique(df_monthly$UID[is.na(df_monthly$precip_mm)])
if (length(UID_nodata) > 0) stop("Still missing data for some fields")

# aggregate to growing season by field
df_gs <-
  df_monthly |> 
  subset(Month %in% gs_months) |> 
  group_by(UID, Year) |> 
  summarize(precip_mm = sum(precip_mm))

# aggregate to year by field
df_yearly <-
  df_monthly |>
  group_by(UID, Year) |>
  summarize(precip_mm = sum(precip_mm))

# check to make sure all fields have data
if (sum(is.na(df_monthly$precip_mm)) > 0) { stop("error - missing fields in monthly") }
if (sum(is.na(df_gs$precip_mm)) > 0) { stop("error - missing fields in gs") }
if (sum(is.na(df_yearly$precip_mm)) > 0) { stop("error - missing fields in yearly") }

# save output
write_csv(df_yearly, file.path("data", "RadarPrecip_AnnualByField.csv"))
write_csv(df_gs, file.path("data", "RadarPrecip_GrowingSeasonByField.csv"))
write_csv(df_monthly, file.path(dir_r, "RadarPrecip_MonthlyByField.csv"))

# visualize output
sf_fields_reproj |> 
  left_join(subset(df_yearly, Year == 2017), by = "UID") |> 
  ggplot(aes(fill = precip_mm)) + 
  geom_sf()
