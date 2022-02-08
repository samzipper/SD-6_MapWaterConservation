## OpenET_PrepFieldShapefile_SplitForAPI.R
# This loads the field boundary shapefile and splits into individual shapefiles 
# totaling <1500 fields each, which is the max allowed by the OpenET API.

library(sf)
library(tidyverse)

# load shapefiles
fields_all <- st_read(file.path("data", "Fields_Boundaries.gpkg"))
SD6 <- st_read(file.path("data", "SD6_outline.gpkg"))
buffer <- st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# there are some duplicated UIDs (fields split into multiple polygons)
# for each, retain the larger area - they are tiny so negligible overall
fields_nodups <- 
  fields_all %>% 
  mutate(area = st_area(.)) %>% 
  group_by(UID) %>% 
  filter(area == min(area))

length(unique(fields_nodups$UID)) == dim(fields_nodups)[1]

st_write(fields_nodups, file.path("data", "Fields_NoDups.shp"))

# reduce the buffer area
domain <- 
  st_union(SD6, buffer) %>% 
  st_transform(st_crs(fields_all))

ggplot() +
  geom_sf(data = fields_nodups) +
  geom_sf(data = domain, color = "red")

# domain still has some slivers, but that's OK since we will just be using a buffer operation

buffer_dist <- 1000 # units are meters
fields_keep <- 
  st_is_within_distance(fields_nodups, domain, dist = buffer_dist, sparse = F) %>% 
  c()

fields_subset <- fields_nodups[fields_keep, ]

# plot
ggplot() +
  geom_sf(data = fields_subset) +
  geom_sf(data = domain, color = "red", fill = NA)

# figure out how to split based on max allowed number of fields and acres. use 80% of actual quota just in case
n_mo <- 6*12  # number of total months of data. 6 years (2016-2021), 12 months each
quota_fields <- 1500*0.8
quota_area <- 50000*4046.85642*0.8 # 50k acres converted to m^2
quota_areamo <- 1776089*4046.85642*0.8/n_mo  # acre-months converted to  m2

n_from_fields <- ceiling(dim(fields_subset)[1]/quota_fields)
n_from_area <- ceiling(sum(fields_subset$area)/min(c(quota_area, quota_areamo)))

n_files <- max(c(n_from_fields, n_from_area)) + 1  # add 1 just in case

# create new column defining split
groupvec <- rep(seq(1, n_files), quota_fields)
fields_subset$group <- groupvec[1:length(fields_subset$UID)]

table(fields_subset$group)

fields_subset %>% 
  group_by(group) %>% 
  summarize(totalarea = sum(area))

# plot
ggplot() +
  geom_sf(data = fields_subset, aes(fill = factor(group)))

# loop through groups and save individual shapefiles
for (i in 1:n_files){
  fields_group <- 
    fields_subset %>% 
    subset(group == i) %>% 
    dplyr::select(UID, geom)
  
  st_write(fields_group, file.path("data", paste0("Fields_SplitForAPI_Group", i, ".shp")), append = F)
}