## MonthlyWaterBalance.R
# This script is supposed to calculate a simple monthly soil water balance for the overall LEMA (sum of all fields).
#  Water Balance = Precip - ET

source(file.path("code", "paths+packages.R"))

# load field attributes - LEMA or no?
fields_spatial <- 
  read_csv(file.path("data", "Fields_Attributes-Spatial.csv"))

# Load monthly met and irrigation data
df_met_mo <- read_csv(file.path(dir_data, "gridMET", "gridmet_MonthlyByField.csv"))
df_et_mo <- read_csv(file.path(dir_openet, "ET_Monthly_All_FieldsNoDups.csv")) |> 
  pivot_longer(starts_with("ET_mm_"), values_to = "ET_mm") |> 
  mutate(Year = year(Date),
         Month = month(Date),
         Algorithm = str_sub(name, start = 12)) |> 
  select(-Date, -name)

# join and subset to only fields of interest
df_et.met_mo <- 
  df_et_mo |> 
  left_join(df_met_mo, by = c("Year", "Month", "UID")) |> 
  left_join(fields_spatial, by = "UID") |> 
  subset(within_lema)

# calculate total lema volume
df_et.met_mo$P.ET_mm <- df_et.met_mo$precip_mm - df_et.met_mo$ET_mm
df_et.met_mo$P.ET_m3 <- (df_et.met_mo$P.ET_mm/1000)*df_et.met_mo$area_m2

df_wb.mo_lema <- 
  df_et.met_mo |> 
  group_by(Algorithm, Year, Month) |> 
  summarize(LEMA_P.ET_m3 = sum(P.ET_m3)) |> 
  pivot_wider(names_from = "Algorithm", names_prefix = "P.ET_m3_", values_from = "LEMA_P.ET_m3")

# calculate cumulative sum water balance for each algorithm
df_wb.mo_lema$WB_m3_ensemble <- cumsum(df_wb.mo_lema$P.ET_m3_ensemble)
df_wb.mo_lema$WB_m3_disalexi <- cumsum(df_wb.mo_lema$P.ET_m3_disalexi)
df_wb.mo_lema$WB_m3_eemetric <- cumsum(df_wb.mo_lema$P.ET_m3_eemetric)
df_wb.mo_lema$WB_m3_geesebal <- cumsum(df_wb.mo_lema$P.ET_m3_geesebal)
df_wb.mo_lema$WB_m3_ptjpl <- cumsum(df_wb.mo_lema$P.ET_m3_ptjpl)
df_wb.mo_lema$WB_m3_sims <- cumsum(df_wb.mo_lema$P.ET_m3_sims)
df_wb.mo_lema$WB_m3_ssebop <- cumsum(df_wb.mo_lema$P.ET_m3_ssebop)

# convert to long form
df_wb_plot <-
  df_wb.mo_lema |> 
  pivot_longer(starts_with("WB_m3_"), values_to = "WB_m3") |> 
  mutate(Date = ymd(paste0(Year, "-", Month, "-01")),
         Algorithm = str_sub(name, start = 7))

# plot
gs_rect <- data.frame(left = ymd(paste0(seq(2016, 2021), "-05-01")),
                      right = ymd(paste0(seq(2016, 2021), "-10-31")),
                      bottom = -Inf,
                      top = Inf)

p_wb <-
  ggplot() +
  geom_rect(data = gs_rect, aes(xmin = left, xmax = right, ymin = bottom, ymax = top),
            fill = col.gray, alpha = 0.25) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line(data = df_wb_plot, aes(x = Date, y = WB_m3/1e6, color = Algorithm)) +
  scale_y_continuous(name = "Cumulative soil water balance (P-ET) [x10\u2076 m\u00b3]") +
  scale_x_date(name = "Date [monthly resolution]", expand = c(0,0)) +
  scale_color_manual(values = pal_algorithms, labels = labs_algorithms) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
ggsave(file.path("figures+tables", "MonthlyWaterBalance.png"),
       p_wb, width = 120, height = 120, units = "mm")
