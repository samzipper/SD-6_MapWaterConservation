## CropType+IrrigatedArea_Timeseries.R
# Plot crop type and irrigated extent

source(file.path("code", "paths+packages.R"))

## load data
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))
fields_irrigation <- 
  read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5,
         IrrConfidence = "Unknown")
fields_landcover <- 
  read_csv(file.path("data", "Fields_Attributes-LandCover-AnnualCDL.csv")) |> 
  dplyr::left_join(crop_names.groups, by = "CropCode")

# join and subset to years of interest (matching IrrigatedAreaTimeseries.R)
yrs_plot <- seq(2006, 2020)
fields_attributes <- 
  left_join(fields_irrigation, fields_landcover, by = c("Year", "UID")) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(Year %in% yrs_plot)

## summarize area for different crop types, irrigation status
crops_summary_all <-
  fields_attributes |> 
  group_by(Year, CropGroupCoarse, within_lema) |> 
  summarize(area_m2_crop = sum(area_m2)) |> 
  # get rid of minor crop types
  subset(!(CropGroupCoarse %in% c("Developed", "Wetland")))

crops_summary_irr <-
  fields_attributes |> 
  subset(Irrigation) |> 
  group_by(Year, CropGroupCoarse, within_lema) |> 
  summarize(area_m2_crop = sum(area_m2)) |> 
  # get rid of minor crop types
  subset(!(CropGroupCoarse %in% c("Developed", "Wetland")))

irr_summary <-
  fields_attributes |> 
  subset(Irrigation & 
           (within_lema | within_buffer)) |> 
  group_by(Year, within_lema) |> 
  summarize(IrrArea_m2 = sum(area_m2))

# main text: line plots, ratio of irrigated sorghum/corn and irrigation status
crop_ratio_irr <-
  crops_summary_irr |> 
  subset(CropGroupCoarse %in% c("Corn", "Sorghum")) |> 
  pivot_wider(id_cols = c("Year", "within_lema"), names_from = "CropGroupCoarse", values_from = "area_m2_crop")

p_ratio <-
  ggplot(crop_ratio_irr,
       aes(x = Year, y = Corn/Sorghum, color = within_lema)) +
  geom_vline(xintercept = 2012.5, color = "black", linetype = "dashed") +
  geom_line() +
  geom_point() +
  scale_x_continuous(name = NULL, breaks = seq(2006, 2021, 3)) +
  scale_y_continuous(name = "Corn/Sorghum Irrigated\nArea Ratio") +
  scale_color_manual(name = "Location", values = c(col.cat.blu, col.cat.red),
                     labels = c("Buffer", "LEMA")) +
  theme(legend.position = c(0.85, 0.75))

p_irr <-
  ggplot(irr_summary, aes(x = Year, y = IrrArea_m2/1e7, color = within_lema)) +
  geom_vline(xintercept = 2012.5, color = "black", linetype = "dashed") +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(2006, 2021, 3)) +
  scale_y_continuous(name = "Irrigated Area\n[x1000 ha]") +
  scale_color_manual(name = "Location", values = c(col.cat.blu, col.cat.red),
                     labels = c("Buffer", "LEMA"), guide = FALSE)

p_combo_ts <-
  (p_ratio + p_irr) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag.position = c(0.19, 0.95))
p_combo_ts

ggsave(file.path("figures+tables", "CropType+IrrigatedArea_Timeseries.png"),
       p_combo_ts, width = 95, height = 110, units = "mm")

# SI: rough breakdown of total crop type, LEMA only, by year
p_LEMA_all <- 
  ggplot(subset(crops_summary_all, within_lema), aes(x = factor(Year), y = area_m2_crop/1e7, fill = CropGroupCoarse)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Land Cover", values = pal_crops_coarse) +
  scale_x_discrete(name = "Year", labels = c("2006", "", "", "2009", "", "", "2012", "", "",
                                             "2015", "", "", "2018", "", "")) +
  scale_y_continuous(name = "Total Area within LEMA [x1000 ha]", expand = c(0,0)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

p_LEMA_irr <- 
  ggplot(subset(crops_summary_irr, within_lema), aes(x = factor(Year), y = area_m2_crop/1e7, fill = CropGroupCoarse)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Land Cover", values = pal_crops_coarse) +
  scale_x_discrete(name = "Year", labels = c("2006", "", "", "2009", "", "", "2012", "", "",
                                             "2015", "", "", "2018", "", "")) +
  scale_y_continuous(name = "Irrigated Area within LEMA [x1000 ha]", expand = c(0,0)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

p_combo_bars <-
  (p_LEMA_all + p_LEMA_irr) +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "CropTypeTimeseries_LEMAareaByYear.png"),
       p_combo_bars, width = 190, height = 95, units = "mm")