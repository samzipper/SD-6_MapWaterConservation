## FlowChart_Maps.R

source(file.path("code", "paths+packages.R"))

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))
irr_fields <- readr::read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) # 1984-2020
lc_fields  <- readr::read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv"))  # 2006-2020

# annual ET and precip
df_met_yr <- readr::read_csv(file.path("data", "gridmet_AnnualByField.csv"))

# estimated irrigation
df_irr <- read_csv(file.path(dir_openet, "OpenET_FieldIrrigation_Annual.csv"))

# years to subset/plot
yr_plot <- 2018

# combine with ET and crop type
sf_all <-
  dplyr::full_join(sf_fields, df_met_yr, by = "UID") %>% 
  dplyr::left_join(subset(df_irr, Algorithm == "ensemble"), by = c("UID", "Year")) %>% 
  subset(Year == yr_plot)

## maps of input data

# figure out extent
# if you want to include buffer use sf_buff; if just lema, use sf_lema here
bound <- sf::st_buffer(st_transform(sf_lema, st_crs(sf_all)), dist = units::set_units(8000, "m"))
extent <- sf::st_bbox(st_transform(sf_lema, st_crs(sf_all)))
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

## make maps
# set panel width and height
map_width <- 60
map_height <- map_width*2/3

p_et <-
  ggplot() +
  geom_sf(data = sf_subset, aes(fill = ET_mm_bound), color = NA) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = NULL, direction = -1, breaks = seq(500, 900, 100), 
                       labels = c("<500", "600", "700", "800", ">900"),
                       option = "C", guide = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  NULL
ggsave(file.path("figures+tables", "Flowchart_Maps-ET.png"),
       p_et, width = map_width, height = map_height, units = "mm")

p_prec <-
  ggplot() +
  geom_sf(data = sf_subset, aes(fill = precip_mm), color = NA) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = NULL, direction = -1, guide = "none", option = "E") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  NULL
ggsave(file.path("figures+tables", "Flowchart_Maps-Precip.png"),
       p_prec, width = map_width, height = map_height, units = "mm")

p_irrStatus <-
  ggplot() +
  geom_sf(data = sf_subset, aes(fill = Irrigation), color = NA) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_manual(name = NULL, values = c(col.cat.yel, col.cat.grn), guide = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  NULL
ggsave(file.path("figures+tables", "Flowchart_Maps-irrStatus.png"),
       p_irrStatus, width = map_width, height = map_height, units = "mm")

p_irrDepth <- 
  ggplot() +
  geom_sf(data = subset(sf_subset, Irrigation), aes(fill = FieldIrrigation_mm), color = NA) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  scale_fill_viridis_c(name = NULL, 
                       direction = -1,
                       option = "C",
                        guide = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  NULL
ggsave(file.path("figures+tables", "Flowchart_Maps-irrDepth.png"),
       p_irrDepth, width = map_width, height = map_height, units = "mm")
