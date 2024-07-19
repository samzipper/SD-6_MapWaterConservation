source(file.path("code", "paths+packages.R"))

df_ETaw_202406 <- 
  read_csv("C:/Users/s947z036/OneDrive - University of Kansas/Research/LEMA_Sheridan-6/data/OpenET/ETaw_20240604extraction/LEMAarea_ETaw_mean_vals.csv") |> 
  rename(Date = image_date, ETaw_mm_mean_ensemble = mean) |> 
  dplyr::select(-area) |> 
  mutate(Year = year(Date)) |> 
  subset(Year > 2016)

df_ETaw_202407 <- 
  read_csv("C:/Users/s947z036/OneDrive - University of Kansas/Research/LEMA_Sheridan-6/data/OpenET/ETaw_monthly_sum_mean_070624_3monthSpinUp/ETaw_monthly_sum_mean_070624_3monthSpinUp.csv") |> 
  rename(Date = image_date, ETaw_mm_mean_ensemble = mean) |> 
  dplyr::select(-area) |> 
  mutate(Year = year(Date)) |> 
  subset(Year > 2016)

df_ETaw_both <- left_join(df_ETaw_202406, df_ETaw_202407, 
                          by = c("UID", "Date", "Year"),
                          suffix = c("_202406", "_202407"))

ggplot(df_ETaw_both, aes(x = ETaw_mm_mean_ensemble_202406, y = ETaw_mm_mean_ensemble_202407)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  facet_wrap(~Year)
