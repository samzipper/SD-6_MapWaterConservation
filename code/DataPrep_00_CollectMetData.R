## DataPrep_00_CollectAnnualMetData.R
# This script is intended to collect meteorological data for each field
# from gridMET which was downloaded from Google Earth Engine.

source(file.path("code", "paths+packages.R"))

# load daily met data
df_daily <-
  file.path(dir_data, "gridMET", "gridMET_FieldsNoDups_500m_2016.csv") |>
  read_csv() |>
  mutate(UID = as.integer(UID),
         date_ymd = ymd(date_ymd),
         Year = year(date_ymd),
         Month = month(date_ymd))

# some tiny fields do not have data - find the nearest one
sf_fields <- st_read(file.path("data", "Fields_NoDups.shp"))
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
  group_by(UID, Year) |> 
  summarize(precip_mm = sum(precip_mm),
            ETr_mm = sum(ETr_mm),
            ETo_mm = sum(ETo_mm))

# aggregate to water year by field
df_wyear <-
  df_monthly |> 
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
write_csv(df_wyear, file.path("data", "gridmet_WaterYearByField.csv"))
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