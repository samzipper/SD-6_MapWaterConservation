## Fig_DeepPerc-GloseEtAl.R
# This script usesdata from Glose et al (2022): https://osf.io/8vmkg
# Plots the relationship between precipitation and deep percolation.

source(file.path("code", "paths+packages.R"))

# load data
df <- read_csv(file.path("data", "GloseEtAl-PPT_Relationship_Data.csv"))
names(df)

lm_dpVprecip <- lm(`Ann LEMA DP (mm)` ~ `PPT Ann (mm)`, data = df)

ggplot(df, aes(x = `PPT Ann (mm)`, y = `Ann LEMA DP (mm)`)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Annual Precipitation [mm]",
       y = "Annual Deep Percolation [mm]")
ggsave(file.path("figures+tables", "Fig_DeepPerc-GloseEtAl.png"),
       width = 95, height = 95, units = "mm")

summary(lm_dpVprecip)
