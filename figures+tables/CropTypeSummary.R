## CropTypeSummary.R
# Bar plot showing crop type by area and year

source(file.path("code", "paths+packages.R"))

## load data
# land cover and irrigation status (from DataPrep_01_Fields_SeparateBoundaries+Attributes.R)
irr_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Irrigation.csv")) %>% 
  pivot_longer(-UID, names_prefix = "irr", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "Irrigation")
lc_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-LandCover.csv")) %>% 
  dplyr::select(-starts_with("pctcov")) %>% 
  pivot_longer(-UID, names_prefix = "cls", names_to = "Year", names_transform = list(Year = as.numeric), values_to = "CropCode")
att_fields <- 
  readr::read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

## combine
fields_attributes <-
  dplyr::left_join(irr_fields, lc_fields, by = c("UID", "Year")) %>% 
  dplyr::left_join(att_fields, by = c("UID")) %>% 
  subset(within_lema | within_buffer) %>% 
  dplyr::left_join(crop_names.groups, by = "CropCode")

## summarize and plot
area_m2_allfields <- sum(subset(att_fields, within_lema | within_buffer)$area_m2) # area of all fields

# first, just get rough breakdown of total crop type
crops_summary <-
  fields_attributes %>% 
  dplyr::group_by(Year, CropGroup) %>% 
  dplyr::summarize(area_m2_crop = sum(area_m2),
                   area_prc_crop = area_m2_crop/area_m2_allfields)

ggplot(crops_summary, aes(x = factor(Year), y = area_prc_crop, fill = CropGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Land Cover", values = pal_crops) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Area [% of total]", limits = c(0, 1), expand = c(0,0), labels = scales::percent) +
  ggsave(file.path("figures+tables", "CropTypeSummary_AreaByYear.png"),
         width = 190, height = 120, units = "mm")

# for widespread land covers, break down by irrigation status and within/outside lema
crops_main <- c("Corn", "Fallow/Idle Cropland", "Grass/Shrub", "Sorghum", "Soybeans", "Winter Wheat")

# we want to know the area for each crop type, irrigated and non-irrigated, within lema and buffer for each year
crops_main_summary <-
  fields_attributes %>% 
  subset(CropGroup %in% crops_main) %>% 
  dplyr::group_by(Year, within_lema, Irrigation, CropGroup) %>% 
  dplyr::summarize(area_m2_crop = sum(area_m2))

ggplot(crops_main_summary, aes(x = factor(Year), y = area_m2_crop/10000, fill = CropGroup)) +
  geom_bar(stat = "identity") +
  facet_grid(within_lema ~ Irrigation, 
             labeller = as_labeller(c("0" = "Non-Irrigated", "1" = "Irrigated", 
                                      "TRUE" = "LEMA", "FALSE" = "Buffer"))) +
  scale_fill_manual(name = "Land Cover", values = pal_crops) +
  scale_x_discrete(name = "Year", labels = c("2006", "", "", "2009", "", "", "2012", "", "", "2015", "", "", "2018")) +
  scale_y_continuous(name = "Area [ha]") +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures+tables", "CropTypeSummary_MainCrops.png"),
         width = 190, height = 190, units = "mm")
  