## IrrigatedAreaTimeseries.R
# Make a time series of irrigated area for the LEMA and the buffer area.

source(file.path("code", "paths+packages.R"))

# load field attributes and irrigation
df_irr <- read_csv(file.path("data", "Fields_Attributes-Irrigation-AnnualAIM.csv"))

df_att <- read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

df_irr.att <- left_join(df_irr, df_att, by = "UID")

# subset to fields of interest and sum by year and lema/buffer
df_totals <-
  df_irr.att |> 
  subset(IrrigatedPrc >= 0.5 & 
           (within_lema | within_buffer)) |> 
  group_by(Year, within_lema) |> 
  summarize(IrrArea_m2 = sum(area_m2))

# plot
yrs_plot <- seq(2005, 2020)

# line plot
ggplot(subset(df_totals, Year %in% yrs_plot), 
       aes(x = Year, y = IrrArea_m2/10000, color = within_lema)) +
  geom_vline(xintercept = 2012.5, color = "black", linetype = "dashed") +
  geom_point() +
  geom_line() +
  scale_x_continuous() +
  scale_y_continuous(name = "Irrigated Area [ha]") +
  scale_color_manual(name = "Location", values = c(col.cat.blu, col.cat.red),
                     labels = c("Buffer", "LEMA")) +
  theme(legend.position = c(0.85, 0.13))

ggsave(file.path("figures+tables", "IrrigatedAreaTimeseries.png"),
       width = 95, height = 95, units = "mm")
