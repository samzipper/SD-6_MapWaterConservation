## FieldComp_TD.R
# Field-resolution comparison for field from TD. Data are not to be publicly shared
# so are not in the GitHub repo.

source(file.path("code", "paths+packages.R"))

path_to_data <- file.path("G:/.shortcut-targets-by-id/1fM3-4oKs6lEiTg-VQECNObDmlw9jVdX3/EGGS/NASA OpenET/data/field-specific water use")

yr_start <- 2017
yr_end <- 2021

# load OpenET data
df_openet <- read_csv(file.path(path_to_data, "TD_OpenET_Monthly-Clean.csv")) |> 
  mutate(MonthYear = mdy(MonthYear)) |> 
  subset(year(MonthYear) >= yr_start & year(MonthYear) <= yr_end)
df_field <- read_csv(file.path(path_to_data, "TD_FieldData_Monthly.csv")) |> 
  mutate(MonthYear = mdy(MonthYear),
         irrigation_mm = irrigation_inches*25.4) |> 
  subset(year(MonthYear) >= yr_start & year(MonthYear) <= yr_end)

# pivot and join
#  MonthYear, ET, Precip, ET-Precip
df_long <- 
  df_openet |> 
  dplyr::select(-eto_mm) |> 
  pivot_longer(cols = -c("MonthYear", "precip_mm"),
               names_to = "algorithm", values_to = "ET_mm") |> 
  mutate(ET.P_mm = ET_mm - precip_mm,
         irrigation_mm = NA) |> 
  left_join(df_field, by = "MonthYear", suffix = c("", ".reported"))

# calculate estimated irrigation
df_long$irrigation_mm <- df_long$ET.P_mm
df_long$irrigation_mm[df_long$irrigation_mm < 0] <- 0 # min allowed irrigation depth: 0 mm
df_long$irrigation_mm[!(month(df_long$MonthYear) %in% seq(3,11))] <- 0  # no winter irrigation

# calculate cumulative sum
algs <- unique(df_long$algorithm)
for (a in algs){
  df_a <- 
    df_long |> 
    subset(algorithm == a) |> 
    arrange(MonthYear) |> 
    mutate(irrigation_mm.cum = cumsum(irrigation_mm))
  
  if (a == algs[1]){
    df_cumsum <- df_a
  } else {
    df_cumsum <- bind_rows(df_cumsum, df_a)
  }
}

df_field$irrigation_mm.cum <- cumsum(df_field$irrigation_mm)

## plots
# time series plot
p_ts <- 
  ggplot() +
  geom_line(data = df_long, aes(x = MonthYear, y = irrigation_mm, color = algorithm)) +
  geom_point(data = df_field, aes(x = MonthYear, y = irrigation_mm)) +
  scale_color_manual(name = "Algorithm", values = pal_algorithms, labels = labs_algorithms) +
  labs(x = "Date [monthly]",
       y = "Irrigation [mm]")
p_ts

# scatterplot
p_xy <-
  ggplot(df_long, aes(y = irrigation_mm, x = irrigation_mm.reported, color = algorithm)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  scale_color_manual(name = "Algorithm", values = pal_algorithms, labels = labs_algorithms) +
  labs(x = "Reported Irrigation [mm]",
       y = "Estimated Irrigation [mm]")
p_xy

# cumsum plot
p_cs <-
  ggplot() +
  geom_line(data = df_cumsum, aes(x = MonthYear, y = irrigation_mm.cum, color = algorithm)) +
  geom_point(data = df_field, aes(x = MonthYear, y = irrigation_mm.cum)) +
  scale_color_manual(name = "Algorithm", values = pal_algorithms, labels = labs_algorithms) +
  labs(x = "Date [monthly]",
       y = "Cumulative Irrigation [mm]")
p_cs

# combine
(p_ts + p_cs) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(title = "Comparison of OpenET (lines) and reported irrigation (dots)",
                  subtitle = "TD Field in GMD3")
ggsave(file.path(path_to_data, "TD_FieldComp_Irr_Neg+Winter0.png"),
       width = 190, height = 150, units = "mm")
