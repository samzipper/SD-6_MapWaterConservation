## paths+packages.R

# packages
library(tidyverse)
library(sf)
library(patchwork)
library(lubridate)
library(hydroGOF)

# common variables
gs_months <- seq(4, 10) # define growing season months

# path to GIS data
dir_data <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/LEMA_Sheridan-6/data"
dir_GIS <- file.path(dir_data, "GIS")
dir_openet <- file.path(dir_data, "OpenET", "Monthly_2016-2021_20221006extraction") # current version of OpenET data

## plotting controls
# ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

## crop type
# based on the file `cls_legend.xlsx` in the dir_GIS/landUse+irrigation which was created by Jude
# get colors from here ('Bands' tab): https://developers.google.com/earth-engine/datasets/catalog/USDA_NASS_CDL#bands
crop_names.groups <- 
  tibble::tibble(CropCode = c(1, 4, 5, 6, 24, 61, 510, 520, 530, 540, 550, 560,
                              21, 23, 25, 27, 28, 29, 205,
                              2, 31, 33, 42, 43, 53,
                              26, 225, 226, 235, 236, 237, 238),
                 CropName = c("Corn", 
                              "Sorghum", 
                              "Soybeans", 
                              "Sunflower", 
                              "Winter Wheat", 
                              "Fallow/Idle", 
                              "Alfalfa/Hay", 
                              "Grass/Shrub", 
                              "Forest", 
                              "Wetland", 
                              "Developed", 
                              "Barren/Water", 
                              "Barley", "Spring Wheat", "Other Small Grains", "Rye", "Oats", "Millet", "Triticale", 
                              "Cotton", "Canola", "Safflower", "Dry Beans", "Potatoes", "Peas", 
                              "Dbl Crop WinWht/Soybeans", "Dbl_Crop_WinWht/Corn", "Dbl Crop Oats/Corn", "Dbl Crop Barley/Sorghum", 
                              "Dbl Crop WinWht/Sorghum", "Dbl_Crop_Barley/Corn", "Dbl Crop WinWht/Cotton"),
                 CropGroup = c("Corn", 
                               "Sorghum", 
                               "Soybeans", 
                               "Sunflower", 
                               "Winter Wheat", 
                               "Fallow/Idle", 
                               "Alfalfa/Hay", 
                               "Grass/Shrub", 
                               "Forest", 
                               "Wetland", 
                               "Developed", 
                               "Barren/Water", 
                               rep("Other Small Grains", 7), 
                               rep("Other Crops", 6), 
                               rep("Double Crop", 7)),
                 CropGroupCoarse = c("Corn", 
                                     "Sorghum", 
                                     "Soybeans", 
                                     "Other Crops", 
                                     "Winter Wheat", 
                                     "Fallow/Idle", 
                                     "Alfalfa/Hay", 
                                     "Grass/Shrub", 
                                     "Grass/Shrub", 
                                     "Wetland", 
                                     "Developed", 
                                     "Wetland", 
                                     rep("Other Crops", 7), 
                                     rep("Other Crops", 6), 
                                     rep("Other Crops", 7)))
pal_crops <- c("Corn" = "#ffd300", 
               "Sorghum" = "#ff9e0a", 
               "Soybeans" = "#267000", 
               "Sunflower" = "#ffff00", 
               "Winter Wheat" = "#a57000", 
               "Fallow/Idle" = "#bfbf77", 
               "Alfalfa/Hay" = "#ffa5e2", 
               "Grass/Shrub" = "#e8ffbf", 
               "Forest" = "#93cc93", 
               "Wetland" = "#7cafaf", 
               "Developed" = "#999999", 
               "Barren/Water" = "#ccbfa3", 
               "Other Small Grains" = "#d69ebc", 
               "Other Crops" = "#00af49", 
               "Double Crop" = "#707000")

pal_crops_coarse <- c("Corn" = "#ffd300", 
                      "Sorghum" = "#ff9e0a", 
                      "Soybeans" = "#267000", 
                      "Winter Wheat" = "#a57000", 
                      "Fallow/Idle" = "#bfbf77", 
                      "Alfalfa/Hay" = "#ffa5e2", 
                      "Grass/Shrub" = "#e8ffbf", 
                      "Wetland" = "#7cafaf", 
                      "Developed" = "#999999", 
                      "Other Crops" = "#00af49")

labs_algorithms <- c("disalexi" = "DisALEXI",
                     "ensemble" = "Ensemble",
                     "ptjpl" = "PT-JPL",
                     "eemetric" = "eeMETRIC",
                     "geesebal" = "geeSEBAL",
                     "sims" = "SIMS",
                     "ssebop" = "SSEBop")

pal_algorithms <- c("disalexi" = "#a6cee3", 
                    "eemetric" = "#1f78b4",
                    "ensemble" = "#e31a1c",
                    "geesebal" = "#b2df8a",
                    "ptjpl" = "#33a02c",
                    "sims" = "#fb9a99",
                    "ssebop" = "#fdbf6f",
                    "Reported" = "black")