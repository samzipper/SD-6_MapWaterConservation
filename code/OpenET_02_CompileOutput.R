## OpenET_02_CompileOutput.R
# This script compiles output from OpenET into a single file for both monthly and annual data.

source(file.path("code", "paths+packages.R"))

# define name of ET algorithms
ETalgs <- c("disalexi", "eemetric", "ensemble", "geesebal", "ptjpl", "sims", "ssebop")

# loop through algorithms
for (alg in ETalgs){
  # read in data
  et_alg_in <- 
    file.path(dir_data, "OpenET", "Monthly_2016-2021", paste0("ET_Monthly_", alg, "_FieldsNoDups.csv")) %>% 
    read_csv()
  
  # reorganize and trim
  et_alg_keep <-
    et_alg_in %>% 
    mutate(ET_mm = round(et_mean, 3)) %>% 
    dplyr::select(UID, time, ET_mm) %>% 
    rename(!!paste0("ET_mm_mean_", alg) := ET_mm, Date = time) # rename using variable
  
  if (alg == ETalgs[1]){
    et_mo_out <- et_alg_keep
  } else {
    et_mo_out <- left_join(et_mo_out, et_alg_keep, by = c("UID", "Date"))
  }
}

# sum to annual
et_yr_out <-
  et_mo_out %>% 
  pivot_longer(starts_with("ET_mm_")) %>% 
  mutate(Year = year(Date),
         Algorithm = str_sub(name, start = 12)) %>% 
  group_by(UID, Year, Algorithm) %>% 
  summarize(ET_mm = round(sum(value), 2))

# save output
write_csv(et_mo_out, file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Monthly_All_FieldsNoDups.csv"))
write_csv(et_yr_out, file.path(dir_data, "OpenET", "Monthly_2016-2021", "ET_Annual_All_FieldsNoDups.csv"))
