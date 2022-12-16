## OpenET_03-EstimateIrrigation+Confidence.R
# This script will estimate irrigation at the field and LEMA scales, and confidence in field-scale irrigation status.

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
  mutate(Irrigation = IrrigatedPrc > 0.5,
         IrrConfidence = "Unknown")
fields_landcover <- 
  read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

# join attributes
fields_attributes <- 
  left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) |> 
  left_join(fields_spatial, by = "UID")

## first irrigation confidence check- look for land covers that are not irrigated mapped as irrigated
#  could indicate either problem with land cover or irrigation status
cropgroup_nonirr <- c("Wetland", "Forest", "Developed", "Barren/Water")  # dput(unique(fields_attributes$CropGroup)) to get all options
which(fields_attributes$Irrigation & fields_attributes$CropGroup %in% cropgroup_nonirr)
UIDs_nonirr_landcover <- unique(fields_attributes$UID[(fields_attributes$Irrigation & fields_attributes$CropGroup %in% cropgroup_nonirr)])

# set all of these to low confidence in irrigation status
fields_attributes$IrrConfidence[fields_attributes$UID %in% UIDs_nonirr_landcover] <- "Low"

# determine how many times each of these fields is in which crop group
UID_yr_nonirr <- 
  fields_attributes |> 
  subset(UID %in% UIDs_nonirr_landcover) |> 
  group_by(UID) |> 
  summarize(nonirr_year = sum(CropGroup %in% cropgroup_nonirr))

# anything that is nonirr 3+ years, set to nonirr all years
UID_nonirr_set0 <- UID_yr_nonirr$UID[UID_yr_nonirr$nonirr_year >= 3]
fields_attributes$Irrigation[fields_attributes$UID %in% UID_nonirr_set0] <- FALSE

## start working with ET data
# loop through timesteps
for (ts in c("Annual", "GrowingSeason", "WaterYear")){
  
  # load ET and met data
  fields_et <- 
    read_csv(file.path(dir_openet, paste0("ET_", ts, "_All_FieldsNoDups.csv")))
  
  fields_met <- 
    read_csv(file.path("data", paste0("gridmet_", ts, "ByField.csv")))
  
  # if using WaterYear: rename to Year so column name is consistent
  if (ts == "WaterYear"){
    colnames(fields_et)[colnames(fields_et) == "WaterYear"] <- "Year"
  }
  
  # join with attributes and calculate precipitation deficit
  fields_alldata <-
    left_join(fields_et, fields_met, by = c("Year", "UID")) |> 
    left_join(fields_attributes, by = c("Year", "UID")) |> 
    mutate(ET.P_mm = ET_mm - precip_mm) |> 
    subset(Year >= yr_start & Year <= yr_end)
  
  ## using ensemble mean, assess confidence in irrigation status for each year for dominant crops
  # loop through crop/year combos
  all_years <- unique(fields_alldata$Year)
  all_crops <- c("Corn", "Sorghum", "Soybeans")
  
  for (c in all_crops){
    for (y in all_years){
      # inspect a single crop/year combo
      et_cropyr <- subset(fields_alldata, Year == y & CropGroupCoarse == c & Algorithm == "ensemble")
      
      # define a crop/irrigation confidence variable for each year
      #  - low  = for irrigated, ET < 50th percentile of non-irrigated ET or IrrigatedPrc < 0.9
      #           for non-irrigated, ET > 50th percentile of irrigated ET or IrrigatedPrc > 0.1
      #  - high = for irrigated, ET > 50th percentile of non-irrigated ET and IrrigatedPrc >= 0.9
      #           for non-irrigated, ET < 50th percentile of irrigated ET and IrrigatedPrc <= 0.1
      q50_irr <- quantile(subset(et_cropyr, Irrigation == 1)$ET.P_mm, 0.5)
      q50_nonirr <- quantile(subset(et_cropyr, Irrigation == 0)$ET.P_mm, 0.5)
      
      # classify confidence
      UID_conf_irr.low <- et_cropyr$UID[et_cropyr$Irrigation == 1 & 
                                          (et_cropyr$IrrigatedPrc < 0.9 | 
                                             et_cropyr$ET.P_mm < q50_nonirr)]
      UID_conf_nonirr.low <- et_cropyr$UID[et_cropyr$Irrigation == 0 & 
                                             (et_cropyr$IrrigatedPrc > 0.1 |
                                                et_cropyr$ET.P_mm > q50_irr)]
      
      UID_conf_irr.high <- et_cropyr$UID[et_cropyr$Irrigation == 1 & 
                                           et_cropyr$IrrigatedPrc >= 0.9 & 
                                           et_cropyr$ET.P_mm >= q50_nonirr]
      UID_conf_nonirr.high <- et_cropyr$UID[et_cropyr$Irrigation == 0 & 
                                              et_cropyr$IrrigatedPrc <= 0.1 & 
                                              et_cropyr$ET.P_mm <= q50_irr]
      
      # add into fields_alldata
      fields_alldata$IrrConfidence[fields_alldata$CropGroupCoarse == c & fields_alldata$Year == y &
                                     fields_alldata$UID %in% c(UID_conf_irr.low, UID_conf_nonirr.low) &
                                     fields_alldata$IrrConfidence == "Unknown"] <- "Low"
      
      fields_alldata$IrrConfidence[fields_alldata$CropGroupCoarse == c & fields_alldata$Year == y &
                                     fields_alldata$UID %in% c(UID_conf_irr.high, UID_conf_nonirr.high) &
                                     fields_alldata$IrrConfidence == "Unknown"] <- "High"
      
    }
  }
  
  ## estimate irrigation
  # set any fields with ET - P < 0 = 0 (negative irrigation impossible)
  fields_alldata$FieldIrrigation_mm <- ifelse(fields_alldata$ET.P_mm < 0, 0, round(fields_alldata$ET.P_mm, 1))
  
  # set any non-irrigated fields = 0 (assumes irrigation status correct)
  fields_alldata$FieldIrrigation_mm[!fields_alldata$Irrigation] <- 0
  
  # calculate irrigation volume: mm --> m3
  #  mm/1000 = m
  #  m*area = m3
  fields_alldata$FieldIrrigation_m3 <- round((fields_alldata$FieldIrrigation_mm/1000)*fields_alldata$area_m2, 1)
  
  ## summarize to LEMA total and save
  df_OpenET_irr_total <-
    fields_alldata |> 
    subset(within_lema) |> 
    group_by(Year, Algorithm) |> 
    summarize(OpenETirrigationLEMA_m3 = sum(FieldIrrigation_m3))
  
  write_csv(df_OpenET_irr_total, file.path("data", paste0("OpenET_LEMAtotalIrrigation_", ts, ".csv")))
  
  ## clean up fields_alldata and save
  fields_alldata_out <-
    fields_alldata |> 
    #subset(within_lema | within_buffer) |> 
    select(UID, Year, Algorithm, Irrigation, IrrConfidence, CropGroupCoarse, ET_mm, ET.P_mm, 
           FieldIrrigation_mm, FieldIrrigation_m3, within_lema, within_buffer)
  
  # too big to save in repo - put in large data directory
  write_csv(fields_alldata_out, file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv"))) 
}