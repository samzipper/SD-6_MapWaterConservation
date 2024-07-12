## Fig10+Fig11_ET.PbyAlgorithmAndIrrStatus.R
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

## load WIMAS data and AIM data to convert to mm
df_wimas <- 
  read_csv(file.path("data", "WRGs_LEMAtotalIrrigation.csv")) |> 
  mutate(Algorithm = "Reported") |> 
  rename(Irrigation_m3 = WIMASirrigationLEMA_m3) |> 
  subset(Year >= 2016 & Year <= 2020)

fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

fields_irrigation <- 
  read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv")) |> 
  mutate(Irrigation = IrrigatedPrc > 0.5,
         IrrConfidence = "Unknown") |> 
  subset(Year %in% df_wimas$Year)

# df_irrArea <- 
#   fields_irrigation |> 
#   left_join(fields_spatial, by = "UID") |> 
#   subset(within_lema & Irrigation) |> 
#   group_by(Year) |> 
#   summarize(irrArea_m2_sum = sum(area_m2))
# 
# df_wimas_irrArea <- 
#   left_join(df_wimas, df_irrArea, by = "Year") |> 
#   mutate(Irrigation_mm = 1000*Irrigation_m3/irrArea_m2_sum)
# 
# ## plot ET-P by year and algorithm
# irrDepthMean <- mean(df_wimas_irrArea$Irrigation_mm)
# irrDepthStd <- sd(df_wimas_irrArea$Irrigation_mm)
# irrDepthMin <- min(df_wimas_irrArea$Irrigation_mm)
# irrDepthMax <- max(df_wimas_irrArea$Irrigation_mm)

# set shaded interval - LEMA average (11") +/- 20%
irrDepthMin <- 11*25.4*0.8
irrDepthMax <- 11*25.4*1.2

## plot ET-Peff by year and algorithm
p_et.peff_mm <-
  ggplot(subset(fields_alldata, Irrigation), aes(x = factor(Year), y = ET.Peff_mm,
                                                 fill = factor(Algorithm, levels = c("ensemble", "disalexi", "eemetric",
                                                                                     "geesebal", "ptjpl", "sims", "ssebop")))) +
  geom_hline(yintercept = 0, color = col.gray) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = irrDepthMin, ymax = irrDepthMax,
           fill = col.gray) +
  geom_boxplot(outlier.shape = 1) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Growing Season ET -\nEffective Precipitation [mm]") +
  scale_fill_manual(name = NULL, values = pal_algorithms, labels = labs_algorithms) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             nrow = 2))
ggsave(file.path("figures+tables", "Fig11_ET.PeffBoxplots_mm.png"),
       p_et.peff_mm, width = 190, height = 100, units = "mm")

# medians for text
subset(fields_alldata, Irrigation) |> 
  group_by(Year, Algorithm) |> 
  summarize(FieldIrrigationPeff_mm_median = median(FieldIrrigationPeff_mm),
            FieldIrrigation_mm_median = median(FieldIrrigation_mm)) |> 
  group_by(Year) |> 
  summarize(IrrigationRangePeff_mm = (max(FieldIrrigationPeff_mm_median) - min(FieldIrrigationPeff_mm_median)),
            IrrigationRange_mm = (max(FieldIrrigation_mm_median) - min(FieldIrrigation_mm_median)))


## Figure 10: density plots, irrigated and rainfed corn
#Our transformation function
scaleFUN1000 <- function(x) x*1000

## mm, Peff version
p_dens_mm_Peff <-
  ggplot(subset(fields_alldata, Algorithm == "ensemble"), aes(x = ET.Peff_mm, fill = Irrigation, color = Irrigation)) +
  geom_vline(xintercept = 0, color = col.gray) +
  annotate("rect", ymin = -Inf, ymax = Inf, 
           xmin = irrDepthMin, 
           xmax = irrDepthMax,
           fill = col.gray) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Year, ncol = 1) +
  scale_x_continuous(name = "Growing Season ET - Effective Precipitation [mm]") +
  scale_y_continuous(name = "Density [x1000]", breaks = seq(0, 0.01, 0.005), labels = scaleFUN1000) +
  scale_color_manual(name = NULL,
                     values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.grn),
                     labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated")) +
  scale_fill_manual(name = NULL,
                    values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.grn),
                    labels = c("FALSE" = "Rainfed", "TRUE" = "Irrigated")) +
  theme(legend.position = "bottom")
ggsave(file.path("figures+tables", "Fig10_Peff_IrrigationDensity_mm.png"),
       p_dens_mm_Peff, width = 95, height = 150, units = "mm")
