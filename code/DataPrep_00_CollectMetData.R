## DataPrep_00_CollectAnnualMetData.R
# This script is intended to collect meteorological data for each field
# from gridMET which was downloaded from Google Earth Engine.

source(file.path("code", "paths+packages.R"))

# load daily met data
df_daily <-
  file.path(dir_data, "gridMET", "gridMET_FieldsNoDups_4000m_2016-2021.csv") |>
  read_csv() |>
  mutate(UID = as.integer(UID),
         date_ymd = ymd(date_ymd),
         Year = year(date_ymd),
         Month = month(date_ymd))

# aggregate to month by field
df_monthly <-
  df_daily |>
  group_by(UID, Year, Month) |>
  summarize(date = max(date_ymd),
            precip_mm = sum(pr),
            ETr_mm = sum(etr),
            ETo_mm = sum(eto))

# aggregate to year by field
df_yearly <-
  df_monthly |>
  group_by(UID, Year) |>
  summarize(precip_mm = sum(precip_mm),
            ETo_mm = sum(ETo_mm),
            ETr_mm = sum(ETr_mm))

# save output
write_csv(df_yearly, file.path("data", "gridmet_AnnualByField.csv"))
write_csv(df_monthly, file.path(dir_data, "gridMET", "gridmet_MonthlyByField.csv"))

## compare to data from tom
df_yearly_fromTom <-
  file.path(dir_data, "gridMET", "DataFromTom", "gridmet_AnnualByField.csv") |>
  read_csv()

df_yearly_compare <-
  left_join(df_yearly, df_yearly_fromTom, by = c("Year", "UID"), suffix = c("_GEE", "_Tom"))

ggplot(df_yearly_compare, aes(x = precip_mm_GEE, y = precip_mm_Tom, color = Year)) +
  geom_point()