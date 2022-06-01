## OpenET_06_CompareToWIMAS-WRGs.R
# This script is supposed to get total irrigation within SD-6 LEMA 
# and compare to WIMAS output.

source(file.path("code", "paths+packages.R"))

alldata_wrg <- readr::read_csv(file.path("data", "WRgroups_AnnualData.csv"))

# set criteria for trimming to only the best matches
wrg_best <-
  alldata_wrg |> 
  subset(n_irrfields == 1 &
           n_IrrConfHigh == 1 &
           abs(IrrArea_m2 - area_m2_wrg)/area_m2_wrg < 0.1)

# compare wrg irrigated area and fields irrigated area
ggplot(subset(wrg_best, Algorithm == "ensemble"), aes(x = IrrArea_m2/10000, y = area_m2_wrg/10000)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  scale_x_continuous(name = "Irrigated area [ha], reported") +
  scale_y_continuous(name = "Irrigated area [ha], inferred") +
  labs(title = "Irrigated area within each water rights group")


# compare wrg irrigation volume to pumped volume
ggplot(wrg_best, aes(x = irr_m3_fromPrec, y = irr_m3_fromWIMAS)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_wrap(~Algorithm)

ggplot(wrg_best, aes(x = irr_m3_fromNonIrr, y = irr_m3_fromWIMAS)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = col.cat.blu) +
  facet_wrap(~Algorithm)