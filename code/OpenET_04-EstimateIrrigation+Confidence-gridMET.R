## OpenET_03_EstimateIrrigation+Confidence-gridMET.R
# This script will estimate irrigation at the field and LEMA scales, and confidence in field-scale irrigation status.
# This script uses gridMET precipitation as input.

source(file.path("code", "paths+packages.R"))

# period to calculate/map
yr_start <- 2016
yr_end <- 2020 # no irrigation status for 2021 so cannot include

## load data
# field attributes
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5)
fields_landcover <- 
  read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

# join attributes
fields_attributes <- 
  left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) |> 
  left_join(fields_spatial, by = "UID")

# load deep percolation regressions
df_lm <- read_csv(file.path("data", "DeepPercRegressions_Summary.csv")) # DP = lmSlope*AnnualPrecip_mm + lmInt

## start working with ET data
# loop through timesteps
for (ts in c("Annual", "GrowingSeason", "WaterYear")){
  
  # load ET and met data
  fields_et <- 
    read_csv(file.path(dir_openet, paste0("ET_", ts, "_All_FieldsNoDups.csv")))
  
  fields_met <- 
    read_csv(file.path("data", paste0("gridmet_", ts, "ByField.csv")))
  
  # calculate effective precipitation
  fields_met$DeepPerc_mm <- df_lm$lmSlope[df_lm$ts == ts]*fields_met$precip_mm + df_lm$lmInt[df_lm$ts == ts]
  fields_met$DeepPerc_mm[fields_met$DeepPerc_mm < 0] <- 0
  fields_met$Peff_mm <- fields_met$precip_mm - fields_met$DeepPerc_mm
  
  # if using WaterYear: rename to Year so column name is consistent
  if (ts == "WaterYear"){
    colnames(fields_et)[colnames(fields_et) == "WaterYear"] <- "Year"
    colnames(fields_met)[colnames(fields_met) == "WaterYear"] <- "Year"
  }
  
  # join with attributes and calculate precipitation deficit
  fields_alldata <-
    left_join(fields_et, fields_met, by = c("Year", "UID")) |> 
    left_join(fields_attributes, by = c("Year", "UID")) |> 
    mutate(ET.P_mm = ET_mm - precip_mm,
           ET.Peff_mm = ET_mm - Peff_mm) |> 
    subset(Year >= yr_start & Year <= yr_end)
  
  ## estimate irrigation
  # set any fields with ET - P < 0 = 0 (negative irrigation impossible)
  fields_alldata$FieldIrrigation_mm <- ifelse(fields_alldata$ET.P_mm < 0, 0, round(fields_alldata$ET.P_mm, 1))
  fields_alldata$FieldIrrigationPeff_mm <- ifelse(fields_alldata$ET.Peff_mm < 0, 0, round(fields_alldata$ET.Peff_mm, 1))
  
  # set any non-irrigated fields = 0 (assumes irrigation status correct)
  fields_alldata$FieldIrrigation_mm[!fields_alldata$Irrigation] <- 0
  fields_alldata$FieldIrrigationPeff_mm[!fields_alldata$Irrigation] <- 0
  
  # calculate irrigation volume: mm --> m3
  #  mm/1000 = m
  #  m*area = m3
  fields_alldata$FieldIrrigation_m3 <- round((fields_alldata$FieldIrrigation_mm/1000)*fields_alldata$area_m2, 1)
  fields_alldata$FieldIrrigationPeff_m3 <- round((fields_alldata$FieldIrrigationPeff_mm/1000)*fields_alldata$area_m2, 1)
  
  ## summarize to LEMA total and save
  df_OpenET_irr_total <-
    fields_alldata |> 
    subset(within_lema) |> 
    group_by(Year, Algorithm) |> 
    summarize(OpenETirrigationLEMA_m3 = sum(FieldIrrigation_m3),
              OpenETirrigationLEMAPeff_m3 = sum(FieldIrrigationPeff_m3))
  
  write_csv(df_OpenET_irr_total, file.path("data", paste0("OpenET_LEMAtotalIrrigation_", ts, ".csv")))
  
  ## clean up fields_alldata and save
  fields_alldata_out <-
    fields_alldata |> 
    #subset(within_lema | within_buffer) |> 
    select(UID, Year, Algorithm, Irrigation, CropGroupCoarse, ET_mm, ET.P_mm, ET.Peff_mm, 
           FieldIrrigation_mm, FieldIrrigation_m3, FieldIrrigationPeff_mm, FieldIrrigationPeff_m3, 
           within_lema, within_buffer)
  
  # too big to save in repo - put in large data directory
  write_csv(fields_alldata_out, file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv"))) 
}
