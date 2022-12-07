## OpenET-CompareToWIMAS-WRGs-AllTimescales.R
# This script is supposed to compare WIMAS output and OpenET estimates of irrigated
# for selected water rights groups. 

source(file.path("code", "paths+packages.R"))

for (ts in c("Annual", "WaterYear", "GrowingSeason")){
  
  # load data
  alldata_wrg <- readr::read_csv(file.path("data", paste0("WRgroups_Summarize-", ts, ".csv")))
  
  # set criteria for trimming to only the best matches
  wrg_best <-
    alldata_wrg# |> 
    #subset(n_irrfields == 1 &
    #         n_IrrConfHigh == 1 &
    #         abs(IrrArea_m2 - area_m2_wrg)/area_m2_wrg < 0.1)
  
  # compare wrg irrigation volume to pumped volume
  p_compare <-
    ggplot(wrg_best, aes(x = irr_m3_fromPrec/1e5, y = irr_m3_fromWIMAS/1e5, color = factor(Year))) +
    geom_abline(intercept = 0, slope = 1, color = col.gray) +
    geom_point(shape = 1) +
    stat_smooth(method = "lm", color = col.cat.blu) +
    facet_wrap(~Algorithm, labeller = as_labeller(labs_algorithms)) +
    scale_x_continuous(name = "Estimated Irrigation [x10^5 m\u00b3]") +
    scale_y_continuous(name = "Reported Irrigation [x10^5 m\u00b3]") +
    #scale_color_manual(name = "Year", values = c("2016" = col.cat.org, "2017" = col.cat.blu, "2018" = col.cat.grn)) +
    NULL
  ggsave(file.path("figures+tables", paste0("OpenET-CompareToWIMAS-WRGs-", ts, "_Scatter.png")),
         p_compare, width = 160, height = 160, units = "mm")
  
  # summarize fit by year and algorithm
  df_fit_ts <- 
    wrg_best |> 
    group_by(Year, Algorithm) |> 
    summarize(RMSE = rmse(irr_m3_fromPrec, irr_m3_fromWIMAS),
              NRMSE = nrmse(irr_m3_fromPrec, irr_m3_fromWIMAS, norm = "maxmin"),
              KGE = KGE(irr_m3_fromPrec, irr_m3_fromWIMAS),
              r = cor(irr_m3_fromPrec, irr_m3_fromWIMAS, use = "complete.obs")) |> 
    mutate(ts = ts)
  
  if (ts == "Annual"){
    df_fit <- df_fit_ts
  } else {
    df_fit <- bind_rows(df_fit, df_fit_ts)
  }
  
}

## plot fit by year and ts
ggplot(subset(df_fit, Algorithm == "ensemble"), aes(x = Year, y = NRMSE, fill = ts)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "Normalized RMSE [%]", expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Timescale of ET-P Aggregation",
                    values = c(col.cat.yel, col.cat.blu, col.cat.red),
                    labels = c("Calendar Year (Jan-Dec)",
                               "Growing Season (Apr-Oct)",
                               "Water Year (Oct-Sep)")) +
  theme(legend.position = "bottom")

# figure out which wrg have good data in # of years
wrg_summary <-
  wrg_best |> 
  subset(Algorithm == "ensemble") |> 
  group_by(WR_GROUP) |> 
  summarize(n_years = n())

# compare wrg irrigated area and fields irrigated area
ggplot(subset(wrg_best, Algorithm == "ensemble"), aes(x = IrrArea_m2/10000, y = area_m2_wrg/10000)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Irrigated area [ha], reported") +
  scale_y_continuous(name = "Irrigated area [ha], inferred") +
  labs(title = "Irrigated area within each water rights group")
