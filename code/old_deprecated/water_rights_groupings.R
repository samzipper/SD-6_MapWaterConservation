## water_rights_groupings.R
#' Look at water rights grouping data from Brownie

source(file.path("code", "paths+packages.R"))

## load data
df <- 
  readr::read_csv(file.path("data", "water_right_groupings_sd6.csv")) %>% 
  subset(sd6_flag != "<Null>") %>% 
  subset(UMW_CODE == "IRR")

# total water rights and points of diversion
length(unique(df$WR_ID))
length(unique(df$`PDIV_ID *`))

## categorize into easy, medium, hard
df$grouping <- "unknown"
df$grouping[df$Grp_pd_cnt == 1 & df$Grp_wr_cnt == 1] <- "easy"
df$grouping[df$Grp_pd_cnt > 1 & df$Grp_wr_cnt > 1] <- "really hard"
df$grouping[(df$Grp_pd_cnt > 1) | (df$Grp_wr_cnt > 1)] <- "hard"

table(df$grouping)

## plot diversion locations
ggplot(df, aes(x=LONGITUDE, y=LATITUDE, color=sd6_flag, shape=grouping)) +
  geom_point(size=2) +
  scale_shape_manual(values=c("easy"=16, "hard"=1)) +
  ggsave(file.path("results", "water_rights_groupings_map.png"),
         width=8, height=6, units="in")

sum(df$grouping=="easy" & df$sd6_flag=="SD6")
sum(df$grouping=="easy" & df$sd6_flag=="outside")

sum(df$Grp_wr_cnt == 1 & df$sd6_flag=="SD6")
sum(df$Grp_wr_cnt == 1 & df$sd6_flag=="outside")
