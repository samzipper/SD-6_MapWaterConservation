# Met_AnnualPrecipEToETr.R
# Make a table of annual meteorological data

source(file.path("code", "paths+packages.R"))

readr::read_csv(file.path("data", "gridmet_MonthlyByGridmet.csv")) %>% 
  dplyr::mutate(Year = lubridate::year(datetime)) %>% 
  dplyr::group_by(Year, gridmet_id) %>% 
  dplyr::summarize(precip_mm_gmsum = sum(precip_mm),
                   ETo_mm_gmsum = sum(ETo_mm),
                   ETr_mm_gmsum = sum(ETr_mm)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(precip_mm_mean = mean(precip_mm_gmsum),
                   ETo_mm_mean = mean(ETo_mm_gmsum),
                   ETr_mm_mean = mean(ETr_mm_gmsum)) %>% 
  readr::write_csv(file.path("figures+tables", "Met_AnnualPrecipEToETr.csv"))
