## OpenET_CompareAlgorithmsToEnsemble.R

source(file.path("code", "paths+packages.R"))

# load monthly OpenET data
df_et_mo <- read_csv(file.path(dir_openet, "ET_Monthly_All_FieldsNoDups.csv"))

# switch to long format
df_et_long <- 
  df_et_mo |> 
  pivot_longer(cols = -c("UID", "Date", "ET_mm_mean_ensemble"),
               values_to = "ET_mm_algorithm") |> 
  mutate(Algorithm = str_sub(name, start = 12),
         Year = year(Date),
         Month = month(Date))
  
# join with field data
fields_spatial <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5)
fields_landcover <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

df_merged <-
  left_join(df_et_long, fields_irrigation, by = c("UID", "Year")) |> 
  left_join(fields_landcover, by = c("Year", "UID")) |> 
  left_join(fields_spatial, by = "UID")

# plot
df_plot <- 
  df_merged |> 
  subset(Month %in% seq(5, 10) &
           Irrigation &
           within_lema &
           CropName == "Corn")

ggplot(df_plot, aes(x = ET_mm_mean_ensemble, y = ET_mm_algorithm, color = Algorithm)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point()

# fit stats
getR2 <- function(x, y) summary(lm(y~x))$r.squared
getSlope <- function(x, y) coefficients(lm(y~x))[2]

df_plot |> 
  group_by(Algorithm) |> 
  summarize(RMSD = rmse(ET_mm_mean_ensemble, ET_mm_algorithm),
            R2 = getR2(ET_mm_mean_ensemble, ET_mm_algorithm),
            slope = getSlope(ET_mm_mean_ensemble, ET_mm_algorithm))

  