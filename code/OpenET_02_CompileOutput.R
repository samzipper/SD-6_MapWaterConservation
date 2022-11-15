## OpenET_02_CompileOutput.R
# This script compiles output from OpenET into a single file for monthly, growing season, and annual data.

source(file.path("code", "paths+packages.R"))

# define name of ET algorithms
ETalgs <- c("disalexi", "eemetric", "ensemble", "geesebal", "ptjpl", "sims", "ssebop")

dir_openet <- file.path(dir_data, "OpenET", "Monthly_2016-2021_20221006extraction")

# loop through algorithms
for (alg in ETalgs){
  # read in data
  et_alg_in <- 
    file.path(dir_openet, paste0("ET_Monthly_", alg, "_FieldsNoDups.csv")) |> 
    read_csv()
  
  # reorganize and trim
  et_alg_keep <-
    et_alg_in |> 
    mutate(ET_mm = round(et_mean, 3)) |> 
    dplyr::select(UID, time, ET_mm) |> 
    rename(!!paste0("ET_mm_mean_", alg) := ET_mm, Date = time) # rename using variable
  
  if (alg == ETalgs[1]){
    et_mo_out <- et_alg_keep
  } else {
    et_mo_out <- left_join(et_mo_out, et_alg_keep, by = c("UID", "Date"))
  }
}

# sum to growing season
et_gs_out <-
  et_mo_out |> 
  pivot_longer(starts_with("ET_mm_")) |> 
  mutate(Year = year(Date),
         Month = month(Date),
         Algorithm = str_sub(name, start = 12)) |> 
  subset(Month %in% gs_months) |> 
  group_by(UID, Year, Algorithm) |> 
  summarize(ET_mm = round(sum(value), 2))

# sum to water year
et_wyear_out <-
  et_mo_out |> 
  pivot_longer(starts_with("ET_mm_")) |> 
  mutate(WaterYear = year(Date + days(92)),
         Month = month(Date),
         Algorithm = str_sub(name, start = 12)) |> 
  group_by(UID, WaterYear, Algorithm) |> 
  summarize(ET_mm = round(sum(value), 2))

# sum to annual
et_yr_out <-
  et_mo_out |> 
  pivot_longer(starts_with("ET_mm_")) |> 
  mutate(Year = year(Date),
         Algorithm = str_sub(name, start = 12)) |> 
  group_by(UID, Year, Algorithm) |> 
  summarize(ET_mm = round(sum(value), 2))

# save output
write_csv(et_mo_out, file.path(dir_openet, "ET_Monthly_All_FieldsNoDups.csv"))
write_csv(et_gs_out, file.path(dir_openet, "ET_GrowingSeason_All_FieldsNoDups.csv"))
write_csv(et_wyear_out, file.path(dir_openet, "ET_WaterYear_All_FieldsNoDups.csv"))
write_csv(et_yr_out, file.path(dir_openet, "ET_Annual_All_FieldsNoDups.csv"))

# compare growing season and annual
gs_yr <- left_join(subset(et_gs_out, Year == 2016), subset(et_yr_out, Year == 2016), 
                   by = c("UID", "Year", "Algorithm"), suffix = c(".gs", ".yr"))
ggplot(gs_yr, aes(x = ET_mm.gs, y = ET_mm.yr)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  facet_wrap(. ~ Algorithm)
  