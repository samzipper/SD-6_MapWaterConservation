## Fig6+Fig7_ET.PbyAlgorithmAndIrrStatus.R
# This script plots variability in ET-P as a function of algorithm and irrigation status.

source(file.path("code", "paths+packages.R"))

## choose data to use
ts <- "GrowingSeason"
area_m2_thres <- 60703 # 60703 m2 = 15 acres
CropGroup_keep <- c("Corn")  # crop types to include

## load and trim data
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv")) |> 
  dplyr::select(UID, area_m2)
fields_alldata <- 
  read_csv(file.path(dir_openet, paste0("OpenET_FieldIrrigation_", ts, ".csv"))) |> 
  subset(within_lema & CropGroupCoarse %in% CropGroup_keep) |>   # within LEMA only
  left_join(fields_spatial, by = "UID") |> 
  subset(area_m2 > area_m2_thres)

## plot ET-P by year and algorithm
p_et.p_mm <-
  ggplot(subset(fields_alldata, Irrigation), aes(x = factor(Year), y = ET.P_mm, fill = Algorithm)) +
  geom_hline(yintercept = 0, color = col.gray) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = 11*25.4*0.8, ymax = 11*25.4*1.2,
           fill = col.gray) +
  geom_boxplot(outlier.shape = 1) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Growing Season ET - Precipitation [mm]") +
  scale_fill_manual(values = pal_algorithms, labels = labs_algorithms) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             nrow = 3))
ggsave(file.path("figures+tables", "Fig6_ET.Pboxplots_mm.png"),
       p_et.p_mm, width = 95, height = 130, units = "mm")

p_et.p_in <-
  ggplot(subset(fields_alldata, Irrigation), aes(x = factor(Year), y = ET.P_mm/25.4, fill = Algorithm)) +
  geom_hline(yintercept = 0, color = col.gray) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = 11*0.8, ymax = 11*1.2,
           fill = col.gray) +
  geom_boxplot(outlier.shape = 1) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Growing Season ET - Precipitation [in]") +
  scale_fill_manual(values = pal_algorithms, labels = labs_algorithms) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             nrow = 3))
ggsave(file.path("figures+tables", "Fig6_ET.Pboxplots_in.png"),
       p_et.p_in, width = 95, height = 130, units = "mm")

## Figure 7: density plots, irrigated and rainfed corn
#Our transformation function
scaleFUN1000 <- function(x) x*1000

p_dens_mm <-
  ggplot(subset(fields_alldata, Algorithm == "ensemble"), aes(x = ET.P_mm, fill = Irrigation, color = Irrigation)) +
  geom_vline(xintercept = 0, color = col.gray) +
  annotate("rect", ymin = -Inf, ymax = Inf, 
           xmin = 11*25.4*0.8, xmax = 11*25.4*1.2,
           fill = col.gray) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Year, ncol = 1) +
  scale_x_continuous(name = "Growing Season ET - Precipitation [mm]") +
  scale_y_continuous(name = "Density [x1000]", breaks = seq(0, 0.01, 0.005), labels = scaleFUN1000) +
  scale_color_manual(name = NULL,
                     values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.grn),
                     labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated")) +
  scale_fill_manual(name = NULL,
                    values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.grn),
                    labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated")) +
  theme(legend.position = "bottom")
ggsave(file.path("figures+tables", "Fig7_IrrigationDensity_mm.png"),
       p_dens_mm, width = 95, height = 150, units = "mm")

scaleFUN10 <- function(x) x*10
p_dens_in <-
  ggplot(subset(fields_alldata, Algorithm == "ensemble"), aes(x = ET.P_mm/25.4, fill = Irrigation, color = Irrigation)) +
  geom_vline(xintercept = 0, color = col.gray) +
  annotate("rect", ymin = -Inf, ymax = Inf, 
           xmin = 11*0.8, xmax = 11*1.2,
           fill = col.gray) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Year, ncol = 1) +
  scale_x_continuous(name = "Growing Season ET - Precipitation [in]") +
  scale_y_continuous(name = "Density [x10]", breaks = seq(0, 0.2, 0.1), labels = scaleFUN10) +
  scale_color_manual(name = NULL,
                     values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.grn),
                     labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated")) +
  scale_fill_manual(name = NULL,
                    values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.grn),
                    labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated")) +
  theme(legend.position = "bottom")
ggsave(file.path("figures+tables", "Fig7_IrrigationDensity_in.png"),
       p_dens_in, width = 95, height = 150, units = "mm")
