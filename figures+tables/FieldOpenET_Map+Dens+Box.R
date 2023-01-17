## FieldOpenET_Map+Dens+Box.R

source(file.path("code", "paths+packages.R"))

# choose timescale
ts <- "GrowingSeason"

# lema and buffer
sf_lema <- sf::st_read(file.path("data", "SD6_outline.gpkg"))
sf_buff <- sf::st_read(file.path("data", "SD6-buffer_outline.gpkg"))

# field boundaries and attributes
sf_fields  <- sf::st_read(file.path("data", "Fields_NoDups.shp"))

# figure out extent
bound <- sf::st_buffer(st_transform(sf_buff, st_crs(sf_fields)), dist = units::set_units(8000, "m"))
extent <- sf::st_bbox(st_transform(sf_buff, st_crs(sf_fields)))
sf_subset <- sf_fields[bound, op = st_within]

# load irrigation estimates
df_irr <- read_csv(file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv")))

# set factor order so Ensemble is first
df_irr$Algorithm <- factor(df_irr$Algorithm, levels = c("ensemble", "disalexi", "eemetric", "geesebal",
                                                        "ptjpl", "sims", "ssebop"))

# join with sf for maps
sf_irr_yr <- 
  right_join(sf_subset, df_irr, by = "UID")

## MAPS
# set up facet grid labeller
labs_grid <- c(labs_algorithms, "2016" = "2016", "2017" = "2017", 
               "2018" = "2018", "2019" = "2019", "2020" = "2020")

# map of annual irrigation
p_irr_map <-
  ggplot(subset(sf_irr_yr, Irrigation)) +
  geom_sf(aes(fill = FieldIrrigation_mm), color = NA) +
  geom_sf(data = sf_buff, color = "black", fill = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  facet_grid(Algorithm ~ Year, labeller = as_labeller(labs_grid)) +
  scale_fill_viridis_c(name = "Estimated Irrigation [mm]", 
                       direction = -1,
                       option = "C") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.75)) +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Map_Irrigation.png"),
       p_irr_map, width = 190, height = 210, units = "mm")

p_et.p_map <-
  ggplot(sf_irr_yr) +
  geom_sf(aes(fill = ET.P_mm), color = NA) +
  geom_sf(data = sf_buff, color = "black", fill = NA) +
  geom_sf(data = sf_lema, color = "black", fill = NA) +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"])) +
  facet_grid(Algorithm ~ Year, labeller = as_labeller(labs_grid)) +
  scale_fill_gradient2(name = "ET - Precipitation [mm]",
                       low = col.cat.red, 
                       mid = "white",
                       high = col.cat.blu) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = col.gray),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.75)) +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Map_ET-P.png"),
       p_et.p_map, width = 190, height = 210, units = "mm")

## DENSITY PLOTS - compare among algorithms
p_et_dens <-
  df_irr |> 
  subset(Irrigation & within_lema) |> 
  ggplot() +
  geom_density(aes(x = ET_mm, fill = Algorithm, color = Algorithm), alpha = 0.2) +
  facet_wrap( ~ Year, ncol = 3) +
  scale_x_continuous(name = "ET [mm]",
                     breaks = seq(400, 1000, 200)) +
  scale_y_continuous(name = "Density", expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(labels = labs_algorithms, values = pal_algorithms) +
  scale_fill_manual(labels = labs_algorithms, values = pal_algorithms) +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 2, title.hjust = 0.5)) +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Dens-Alg_ET.png"), 
       p_et_dens, width = 190, height = 95, units = "mm")

p_et.p_dens <-
  df_irr |> 
  subset(Irrigation & within_lema) |> 
  ggplot() +
  geom_vline(xintercept = 0, color = col.gray) +
  annotate("rect", ymin = -Inf, ymax = Inf,
           xmin = (11*25.4 - 11*25.4*0.2), 
           xmax = (11*25.4 + 11*25.4*0.2),
           fill = col.gray, alpha = 0.2) +
  geom_density(aes(x = ET.P_mm, fill = Algorithm, color = Algorithm), alpha = 0.4) +
  facet_wrap( ~ Year, ncol = 3) +
  scale_x_continuous(name = "ET - Precipitation [mm]") +
  scale_y_continuous(name = "Density", expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(labels = labs_algorithms, values = pal_algorithms) +
  scale_fill_manual(labels = labs_algorithms, values = pal_algorithms) +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 2, title.hjust = 0.5)) +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Dens-Alg_ET-P.png"), 
       p_et.p_dens, width = 190, height = 95, units = "mm")

p_et.p_box <-
  df_irr |> 
  subset(CropGroupCoarse == "Corn" & within_lema) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = col.gray) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = (11*25.4 - 11*25.4*0.2), 
           ymax = (11*25.4 + 11*25.4*0.2),
           fill = col.gray, alpha = 0.2) +
  geom_boxplot(aes(x = factor(Year), y = ET.P_mm, fill = Algorithm, color = Algorithm), alpha = 0.4) +
  scale_y_continuous(name = "ET - Precipitation [mm]", expand = c(0,0)) +
  scale_x_discrete(name = "Year") +
  scale_color_manual(labels = labs_algorithms, values = pal_algorithms) +
  scale_fill_manual(labels = labs_algorithms, values = pal_algorithms) +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Box-Alg_ET-P_Corn.png"), 
       p_et.p_box, width = 190, height = 85, units = "mm")

# calculate range of medians for each year: ET - P
df_ET.P_median_comparison <-
  df_irr |> 
  subset(CropGroupCoarse == "Corn" & within_lema) |> 
  group_by(Year, Algorithm) |> 
  summarize(ET.P_mm_median = median(ET.P_mm)) |> 
  pivot_wider(id_cols = "Year", values_from = "ET.P_mm_median", names_from = "Algorithm")
get_range <- function(x) { range(x)[2] - range(x)[1] }
df_ET.P_median_comparison$range_mm <- 
  apply(df_ET.P_median_comparison[, 2:7], MARGIN = 1, FUN = get_range)
  
write_csv(df_ET.P_median_comparison, 
          file.path("figures+tables", "FieldOpenET_Box-Alg_ET-P_Corn_MedianTable.csv"))

# calculate range of medians for each year: ET only
df_ET_median_comparison <-
  df_irr |> 
  subset(CropGroupCoarse == "Corn" & within_lema) |> 
  group_by(Year, Algorithm) |> 
  summarize(ET_mm_median = median(ET_mm)) |> 
  pivot_wider(id_cols = "Year", values_from = "ET_mm_median", names_from = "Algorithm")
df_ET_median_comparison$range_mm <- 
  apply(df_ET_median_comparison[, 2:7], MARGIN = 1, FUN = get_range)

write_csv(df_ET_median_comparison, 
          file.path("figures+tables", "FieldOpenET_Box-Alg_ET_Corn_MedianTable.csv"))

## DENSITY AND BOX PLOTS - comparing irrigated and rainfed
p_et.p_irr_dens <-
  df_irr |> 
  subset(Algorithm == "ensemble" & CropGroupCoarse == "Corn" & within_lema) |> 
  ggplot() +
  geom_vline(xintercept = 0, color = col.gray) +
  annotate("rect", ymin = -Inf, ymax = Inf,
           xmin = (11*25.4 - 11*25.4*0.2), 
           xmax = (11*25.4 + 11*25.4*0.2),
           fill = col.gray, alpha = 0.2) +
  geom_density(aes(x = ET.P_mm, fill = Irrigation, color = Irrigation), alpha = 0.4) +
  facet_wrap( ~ Year, ncol = 3) +
  scale_x_continuous(name = "ET - Precipitation [mm]") +
  scale_y_continuous(name = "Density", expand = expansion(mult = c(0, 0.025))) +
  scale_color_manual(name = "Irrigation Status", 
                     labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated"), 
                     values = c(col.cat.yel, col.cat.grn)) +
  scale_fill_manual(name = "Irrigation Status", 
                    labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated"), 
                    values = c(col.cat.yel, col.cat.grn)) +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Dens-IrrCorn_ET-P.png"), 
       p_et.p_irr_dens, width = 190, height = 95, units = "mm")

p_et.p_irr_box <-
  df_irr |> 
  subset(Algorithm == "ensemble" & CropGroupCoarse == "Corn" & within_lema) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = col.gray) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = (11*25.4 - 11*25.4*0.2), 
           ymax = (11*25.4 + 11*25.4*0.2),
           fill = col.gray, alpha = 0.2) +
  geom_boxplot(aes(x = factor(Year), y = ET.P_mm, fill = Irrigation, color = Irrigation), alpha = 0.4) +
  scale_y_continuous(name = "ET - Precipitation [mm]") +
  scale_x_discrete(name = "Year") +
  scale_color_manual(name = "Irrigation Status", 
                     labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated"), 
                     values = c(col.cat.yel, col.cat.grn)) +
  scale_fill_manual(name = "Irrigation Status", 
                    labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated"), 
                    values = c(col.cat.yel, col.cat.grn)) +
  theme(legend.position = "bottom") +
  NULL
ggsave(file.path("figures+tables", "FieldOpenET_Box-IrrCorn_ET-P.png"), 
       p_et.p_irr_box, width = 95, height = 95, units = "mm")

## BOX PLOTS - compare LEMA and buffer
p_et.p_irr_lemaBufferCorn <-
  df_irr |> 
  subset(CropGroupCoarse == "Corn" & 
           Irrigation & 
           (within_lema | within_buffer)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot(aes(x = Algorithm, y = ET.P_mm, fill = within_buffer, color = within_buffer), alpha = 0.4) +
  facet_wrap( ~ Year, ncol = 3) +
  scale_x_discrete(name = "Algorithm",
                   labels = labs_algorithms) +
  scale_y_continuous(name = "ET - Precipitation [mm]") +
  coord_flip() +
  scale_color_manual(name = "Location", 
                     values = c(col.cat.red, col.cat.blu),
                     labels = c("LEMA", "Buffer")) +
  scale_fill_manual(name = "Location", 
                    values = c(col.cat.red, col.cat.blu),
                    labels = c("LEMA", "Buffer")) +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL

ggsave(file.path("figures+tables", "FieldOpenET_Box-IrrLEMAbuffer_CornET-P.png"), 
       p_et.p_irr_lemaBufferCorn, width = 190, height = 110, units = "mm")

p_irr_lemaBufferCorn <-
  df_irr |> 
  subset(CropGroupCoarse == "Corn" & 
           Irrigation & 
           (within_lema | within_buffer)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot(aes(x = Algorithm, y = FieldIrrigation_mm, fill = within_buffer, color = within_buffer), alpha = 0.4) +
  facet_wrap( ~ Year, ncol = 3) +
  scale_x_discrete(name = "Algorithm",
                   labels = labs_algorithms) +
  scale_y_continuous(name = "Irrigation [mm]") +
  coord_flip() +
  scale_color_manual(name = "Location", 
                     values = c(col.cat.red, col.cat.blu),
                     labels = c("LEMA", "Buffer")) +
  scale_fill_manual(name = "Location", 
                    values = c(col.cat.red, col.cat.blu),
                    labels = c("LEMA", "Buffer")) +
  theme(legend.position = c(0.84, 0.27)) +
  guides(color = guide_legend(ncol = 1, title.hjust = 0.5)) +
  NULL

ggsave(file.path("figures+tables", "FieldOpenET_Box-IrrLEMAbuffer_CornIrr.png"), 
       p_irr_lemaBufferCorn, width = 190, height = 110, units = "mm")

p_et.p_irr_lemaBufferCrops <-
  df_irr |> 
  subset(CropGroupCoarse %in% c("Corn", "Soybeans", "Sorghum") & 
           Irrigation & 
           (within_lema | within_buffer)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot(aes(x = Algorithm, y = ET.P_mm, fill = within_buffer, color = within_buffer), alpha = 0.4) +
  facet_grid(Year ~ CropGroupCoarse) +
  scale_x_discrete(name = "Algorithm",
                   labels = labs_algorithms) +
  scale_y_continuous(name = "ET - Precipitation [mm]") +
  coord_flip() +
  scale_color_manual(name = "Location", 
                     values = c(col.cat.red, col.cat.blu),
                     labels = c("LEMA", "Buffer")) +
  scale_fill_manual(name = "Location", 
                    values = c(col.cat.red, col.cat.blu),
                    labels = c("LEMA", "Buffer")) +
  theme(legend.position = "bottom") +
  NULL

ggsave(file.path("figures+tables", "FieldOpenET_Box-IrrLEMAbuffer_CropsET-P.png"), 
       p_et.p_irr_lemaBufferCrops, width = 190, height = 210, units = "mm")


p_irr_lemaBufferCrops <-
  df_irr |> 
  subset(CropGroupCoarse %in% c("Corn", "Soybeans", "Sorghum") & 
           Irrigation & 
           (within_lema | within_buffer)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot(aes(x = Algorithm, y = FieldIrrigation_mm, fill = within_buffer, color = within_buffer), alpha = 0.4) +
  facet_grid(Year ~ CropGroupCoarse) +
  scale_x_discrete(name = "Algorithm",
                   labels = labs_algorithms) +
  scale_y_continuous(name = "Irrigation [mm]") +
  coord_flip() +
  scale_color_manual(name = "Location", 
                     values = c(col.cat.red, col.cat.blu),
                     labels = c("LEMA", "Buffer")) +
  scale_fill_manual(name = "Location", 
                    values = c(col.cat.red, col.cat.blu),
                    labels = c("LEMA", "Buffer")) +
  theme(legend.position = "bottom") +
  NULL

ggsave(file.path("figures+tables", "FieldOpenET_Box-IrrLEMAbuffer_CropsIrr.png"), 
       p_irr_lemaBufferCrops, width = 190, height = 210, units = "mm")
