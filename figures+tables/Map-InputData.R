## ZipperEtAl_Fig1_MapInputData.R
# This script maps example input data for the SD-6 LEMA.

source(file.path("code", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))
irr_fields <- readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) # 1984-2020
lc_fields  <- readr::read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv"))  # 2006-2020

# annual ET
df_et_yr <- readr::read_csv(file.path(dir_openet, "ET_Annual_All_FieldsNoDups.csv")) # 2016-2021

# years to subset/plot
yr_plot <- 2020

# combine with ET and crop type
sf_all <-
  dplyr::full_join(sf_fields, subset(irr_fields, Year %in% yr_plot), by = c("UID")) %>% 
  dplyr::left_join(lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode") %>% 
  dplyr::left_join(subset(df_et_yr, Algorithm == "ensemble"), by = c("UID", "Year"))

## maps of input data

# figure out extent
# if you want to include buffer use sf_buff; if just lema, use sf_lema here
bound <- sf::st_buffer(st_transform(sf_buff, st_crs(sf_all)), dist = units::set_units(8000, "m"))
extent <- sf::st_bbox(st_transform(sf_buff, st_crs(sf_all)))
sf_subset <- sf_all[bound, op = st_intersects]

quantile(subset(sf_subset, Year %in% yr_plot & Algorithm == "ensemble")$ET_mm, c(0,1))
quantile(subset(sf_subset, Year %in% yr_plot & Algorithm == "ensemble")$ET_mm, c(0.01,0.99))
quantile(subset(sf_subset, Year %in% yr_plot & Algorithm == "ensemble")$ET_mm, c(0.05,0.95))

# cut ET into groups
sf_subset$ET_mm_cut <- cut(sf_subset$ET_mm, 
                           breaks = c(0, seq(550, 850, 100), 1500),
                           labels = c("< 550", "550-650", "650-750", 
                                      "750-850", "> 850"))

# bound so colorbar doesn't get too stretched
sf_subset$ET_mm_bound <- sf_subset$ET_mm
sf_subset$ET_mm_bound[sf_subset$ET_mm < 500] <- 500
sf_subset$ET_mm_bound[sf_subset$ET_mm > 900] <- 900
library("ggspatial") # for scale bar

p_et <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% yr_plot & Algorithm == "ensemble"), aes(fill = ET_mm_bound), color = NA) +
  geom_sf(data = sf_buff, color = col.cat.blu, fill = NA, linewidth = 1) +
  geom_sf(data = sf_lema, color = col.cat.red, fill = NA, linewidth = 1) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = NULL, direction = -1, breaks = seq(500, 900, 100), 
                       labels = c("<500", "600", "700", "800", ">900"),
                       option = "C") +
  annotation_scale(location = "br", text_col = "white", text_family = "Arial") +
  labs(title = "(a) Annual ET") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(barwidth = 10)) +
  NULL

sf_subset$IrrigatedPrc_cut <- cut(sf_subset$IrrigatedPrc, 
                                  breaks = c(0, 0.5, 1), 
                                  labels = c("Rainfed", "Irrigated"),
                                  include.lowest = T)

p_irr <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% yr_plot), aes(fill = IrrigatedPrc_cut), color = NA) +
  geom_sf(data = sf_buff, color = col.cat.blu, fill = NA, linewidth = 1) +
  geom_sf(data = sf_lema, color = col.cat.red, fill = NA, linewidth = 1) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = NULL, values = c(col.cat.yel, col.cat.grn)) +
  labs(title = "(b) Irrigation Status") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0, "pt")) +
  NULL


# lump a few more categories - need to redefine palette
table(subset(sf_subset, Algorithm == "ensemble")$CropGroupCoarse)
sf_subset$CropGroupCoarse[sf_subset$CropGroupCoarse %in% c("Other Crops", "Wetland")] <- "Other"
pal_crops_coarse_lumped <- c("Corn" = "#ffd300", 
                             "Sorghum" = "#ff9e0a", 
                             "Soybeans" = "#267000", 
                             "Winter Wheat" = "#a57000", 
                             "Fallow/Idle" = "#bfbf77", 
                             "Alfalfa/Hay" = "#ffa5e2", 
                             "Grass/Shrub" = "#e8ffbf", 
                             "Developed" = "#999999", 
                             "Other" = "#00af49")
p_lc <-
  ggplot() +
  geom_sf(data = subset(sf_subset, Year %in% yr_plot), aes(fill = CropGroupCoarse), color = NA) +
  geom_sf(data = sf_buff, color = col.cat.blu, fill = NA, linewidth = 1) +
  geom_sf(data = sf_lema, color = col.cat.red, fill = NA, linewidth = 1) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = NULL, values = pal_crops_coarse_lumped, drop = T) +
  labs(title = "(c) Land Cover") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(-10, "pt"),
        legend.margin = margin(0, 0, 0, 0, "pt")) +
  guides(fill = guide_legend(nrow = 3)) +
  NULL

## map of position within Kansas
sf_state <- st_read(file.path("data", "Tiger2010_Census_State.shp"))
sf_hpa <- st_read(file.path("data", "High_Plains_Aquifer_Extent.shp"))

p_state <-
  ggplot() +
  geom_sf(data = sf_hpa, color = NA, fill = col.gray) +
  geom_sf(data = sf_state, color = "black", fill = NA) +
  geom_sf(data = sf_lema, color = col.cat.red, fill = col.cat.red) +
  annotate("text", x = -99.5, y = 38.75, hjust = 0, vjust = 0, 
           label = "High Plains Aquifer", color = "gray45") +
  scale_x_continuous(expand = c(0,0), breaks = seq(-95, -101, -2)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(37, 40, 1)) +
  labs(title = "(d) Position within Kansas") +
  theme(panel.border = element_blank())

## combine and save
p_combo <- 
  (p_et + p_irr + p_lc + p_state) +
  plot_layout(ncol = 2)

ggsave(file.path("figures+tables", "Map-InputData.png"),
       p_combo, width = 190, height = 165, units = "mm")
