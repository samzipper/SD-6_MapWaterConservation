## DataPrep_Fields_01_SeparateBoundaries+Attributes.R
# This script is intended to separate out the field geometry (UID, polygon boundaries) from its characteristics (irrigation status, crop cover).

source(file.path("code", "paths+packages.R"))

# field boundaries
sf_fields <- sf::st_read(file.path(dir_GIS, "landUse+irrigation", "CLU_SPLIT_ZM_inside_SD6buf10mi_CDL_2008_2018.shp"))
sf_fields$area_m2_poly <- as.numeric(sf::st_area(sf_fields))

# lema and buffer
sf_lema <- 
  file.path("data", "SD6_outline.gpkg") %>% 
  sf::st_read() %>% 
  sf::st_transform(sf::st_crs(sf_fields))

sf_buff <- 
  file.path("data", "SD6-buffer_outline.gpkg") %>% 
  sf::st_read() %>% 
  sf::st_transform(sf::st_crs(sf_fields))

# figure out which fields are inside LEMA and buffer
sf_fields$within_lema <- 
  sf::st_intersects(sf_fields, sf_lema) %>% 
  lengths > 0

sf_fields$within_buffer <- 
  sf::st_within(sf_fields, sf_buff) %>% 
  lengths > 0

# there are a handful that are right on the edges which are hard to classify
# manually assign those based on inspecting shapefile in QGIS
# UIDs that are not classified as within buffer, but should (actually either within LEMA or outside buffer)
add_buffer <- c(379297, 1371276, 1228387, 353212, 571399, 350957, 
                1371539, 349674, 353502, 568930,
                350334, 354459, 569053, 349869, 354139, 351538,
                349764, 354342, 377042, 1228248, 1380483, 374222, 374807, 
                375286, 377042, 377144, 377390, 377415, 378190, 378257, 378545, 379332, 1230843, 1244424, 
                1380269, 1380483, 1380890, 1381041, 378471, 348263, 349946, 354479,
                348864, 350509, 1370859, 347879, 353848, 1430641, 1430643, 1430644,
                350042, 352201, 353474, 430682, 1430683, 1430684, 349348, 1430673,
                351435, 1370658, 1371067, 354331, 352240, 353251, 569019, 1371043,
                351638, 352775, 353550, 354013, 571100, 1370569, 1371226,
                349342, 351477, 353924, 569076, 350559, 354685, 569248, 569255,
                1228112, 1370447, 1370922, 1371101, 353917, 569139, 569611, 569617,
                347583, 349544, 569596, 569597, 569598, 352277, 570835,
                570861, 570862, 1371631, 375910, 1379897, 374351, 1379988, 374864, 377530,
                377727, 377787, 378020, 378495, 573941, 1381108, 1431851, 377141, 1380983,
                378816, 1380245) 
sf_fields$within_lema[sf_fields$UID %in% add_buffer] <- FALSE
sf_fields$within_buffer[sf_fields$UID %in% add_buffer] <- TRUE

# check to make sure it is reasonable
ggplot() +
  geom_sf(data = subset(sf_fields, !within_lema & !within_buffer), fill = "black", alpha = 0.5) +
  geom_sf(data = subset(sf_fields, within_lema), fill = "blue", alpha = 0.5) +
  geom_sf(data = subset(sf_fields, within_buffer), fill = "red", alpha = 0.5) +
  geom_sf(data = sf_buff, color = "red", fill = NA) +
  geom_sf(data = sf_lema, color = "blue", fill = NA) +
  coord_sf(expand = F)
sum(sf_fields$within_buffer)
sum(sf_fields$within_lema)
sum(sf_fields$within_buffer & sf_fields$within_lema)

## split out data
# field geometry
fields_boundaries <- 
  sf_fields %>% 
  dplyr::select(UID, geometry)

# field attributes
fields_attributes <- 
  sf_fields %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-Acres)

#note: there are a few (25) fields that have multiple polygons, these are all really small
#      see `dir_GIS/landUse+irrigation/Notes_SD6_CDL_extract.docx` from Jude for more details
#      all data (irr, lc, etc) are the same except for `geometry` and `area_m2_poly`
#      all the ones in SD-6 or the buffer area are small, unimportant in the big picture

# so, for saving output we should calculate area as the sum, and only take unique instances for all other fields

## save
# field boundaries
sf::st_write(fields_boundaries, file.path("data", "Fields_Boundaries.gpkg"), append = F)

# spatial attributes
fields_attributes %>% 
  dplyr::select(UID, area_m2_poly, within_lema, within_buffer) %>% 
  dplyr::group_by(UID, within_lema, within_buffer) %>% 
  dplyr::summarize(area_m2 = sum(area_m2_poly)) %>% 
  dplyr::ungroup() %>% 
  readr::write_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

# Irrigation status
fields_attributes %>% 
  dplyr::select(UID, starts_with("irr")) %>% 
  unique() %>% 
  readr::write_csv(file.path("data", "Fields_Attributes-Irrigation.csv"))

# dominant land cover and percent in that class
fields_attributes %>% 
  dplyr::select(UID, starts_with("cls"), starts_with("pctcov")) %>% 
  unique() %>% 
  readr::write_csv(file.path("data", "Fields_Attributes-LandCover.csv"))
