## TimeToEqVsSlopeChange_LifespanExtension.R
#' Messing around with an idea; making raster plots of how long aquifer lifespan
#' would be extended for different combinations of parameters.

source(file.path("code", "paths+packages.R"))

## build function
calc_lifespan_extension <- 
  function(sat_thick_init, # [m] initial saturated thickness (onset of water conservation)
           sat_thick_min,  # [m] minimum allowed saturated thickness
           slope_init,     # [m/yr] rate of water table decline at year_init
           time_to_eq,     # [yr] time to equilibration
           slope_change,   # [-] percent reduction in slope_init after equilibration
           max_yrs = 500   # [yr] maximum number of years to consider
  ){
    ## assumptions:
    #' - water table decline goes to 0 in year_init, changes instantly to new 
    #'   slope after time_to_eq passes (sharp changes)
    
    ## set up some vectors
    t <- seq(1, max_yrs)     # time in years since conservation began

    ## first: calculate business-as-usual case
    sat_thick_bau <- sat_thick_init - (slope_init*t)  # remaining sat thick in each year [m]
    lifespan_bau <- min(which(sat_thick_bau <= sat_thick_min))  # which year will sat thick drop below min
    
    ## second: calculate conservation case
    sat_thick_cons <- c(rep(sat_thick_init, time_to_eq),  # flat water table during conservation
                        (sat_thick_init - (slope_init*(1-slope_change)*t[1:(length(t)-time_to_eq)]))) # reduced slope once re-equilibrated
    lifespan_cons <- min(which(sat_thick_cons <= sat_thick_min))  # which year will sat thick drop below min
    
    ## return difference
    return(lifespan_cons - lifespan_bau)
  }

## define parameters that won't change
sat_thick_init <- 30  
sat_thick_min <- 9    
slope_init <- 1       
max_yrs <- 200        

## range to vary time_to_eq and slope_change
time_to_eq_all <- seq(0, 50, 1)
slope_change_all <- seq(0, 1, 0.02)

start_flag <- T
for (te in time_to_eq_all){
  for (sc in slope_change_all){
    
    extension_yrs <- calc_lifespan_extension(sat_thick_init = sat_thick_init,
                                             sat_thick_min = sat_thick_min,
                                             slope_init = slope_init,
                                             time_to_eq = te,
                                             slope_change = sc,
                                             max_yrs = max_yrs)
    
    df_extension_i <- tibble::tibble(time_to_eq = te,
                                   slope_change = sc,
                                   lifespan_extension_yrs = extension_yrs)
    
    if (start_flag){
      df_extension <- df_extension_i
      start_flag <- F
    } else {
      df_extension <- dplyr::bind_rows(df_extension, df_extension_i)
    }
  }
}

## plot results
ggplot(df_extension, 
       aes(x = time_to_eq, 
           y = slope_change)) +
  geom_raster(aes(fill = lifespan_extension_yrs)) +
  geom_contour(aes(z = lifespan_extension_yrs),
               binwidth = 10, color = "white") +
  labs(title = "Sensitivity of Extension of Useable Lifespan",
       subtitle = paste0("Initial Saturated Thickness = ", sat_thick_init, " m, Minimum Saturated Thickness = ", sat_thick_min, " m, Initial Slope = ", slope_init, " m/yr\nContours at 10 yr interval, grey shading indicates > 200 yrs")) +
  scale_fill_viridis_c(name = "Useable Lifespan Extension [yrs]") +
  scale_x_continuous(name = "Time to Equilibration [yrs]", expand = c(0, 0)) +
  scale_y_continuous(name = "Slope of Head vs. Year (relative to pre-LEMA) [%]", 
                     expand = c(0, 0), labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  ggsave(file.path("plots", "TimeToEqVsSlopeChange_LifespanExtension.png"),
         width = 190, height = 190, units = "mm")
