## OpenET_05_MakeProcessingDecisions.R
# Need to decide:
#  1. Whether to do rainfed correction or not.
#  2. High confidence irrigation or all fields?
#  3. Whether to do precipitation correction or not.
#  4. What timescale to show (Calendar Year, Water Year, Growing Season)

source(file.path("code", "paths+packages.R"))

# load WIMAS data
df_wimas <- 
  read_csv(file.path("data", "WRGs_LEMAtotalIrrigation.csv")) |> 
  rename(Irrigation_m3 = WIMASirrigationLEMA_m3) |> 
  subset(Year >= 2016 & Year <= 2020)

ts <- "Annual"
df_OpenET <- read_csv(file.path("data", "OpenET_LEMAtotalIrrigation_Annual.csv"))

df_OpenET_long <- 
  df_OpenET |> 
  subset(Algorithm == "ensemble") |> 
  pivot_longer(cols = ends_with("_m3"), values_to = "Irrigation_m3", names_to = "Approach") |> 
  mutate(Irrigation_m3_areaScale = Irrigation_m3*0.64)

df_wimas |> 
  mutate(Approach = "WIMAS", Algorithm = "WIMAS", Irrigation_m3_areaScale = Irrigation_m3) |> 
  bind_rows(df_OpenET_long) |> 
  ggplot(aes(x = Year, y = Irrigation_m3_areaScale, color = Approach)) +
  geom_point() +
  geom_line()

df_OpenET |> 
  left_join(df_wimas, by = "Year") |> 
  ggplot(aes(x = Irrigation_m3, y = OpenETirrigationLEMAPeffRFadj_m3)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  stat_smooth(method = "lm")
