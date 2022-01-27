## OpenET_PrepFieldShapefile_TrimTo10k.R
# This loads the field boundary shapefile and trims to 10,000 fields, which is the max allowed by the OpenET API.

library(sf)
library(tidyverse)

# load shapefiles
fields_all <- st_read(file.path("data", "Fields_Boundaries.gpkg"))
SD6 <- st_read(file.path("data", "SD6_outline.gpkg"))
buffer <- st_read(file.path("data", "SD6-buffer_outline.gpkg"))
domain <- 
  st_union(SD6, buffer) %>% 
  st_transform(st_crs(fields_all))

# plot
ggplot() +
  geom_sf(data = fields_all) +
  geom_sf(data = domain, color = "red")

# domain still has some slivers, but that's OK since we will just be using a buffer operation

buffer_dist <- 5000 # units are meters
fields_keep <- 
  st_is_within_distance(fields_all, domain, dist = buffer_dist, sparse = F) %>% 
  c()

fields_subset <- fields_all[fields_keep, ]

# plot
ggplot() +
  geom_sf(data = fields_subset) +
  geom_sf(data = domain, color = "red", fill = NA)

# save output
st_write(fields_subset, file.path("data", "Fields_SubsetForOpenET.gpkg"))
