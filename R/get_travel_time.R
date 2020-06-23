get_travel_time <- function(sinusfit_sw, sinusfit_gw, retardation_factor = 1.8) {
  
  
  sinusfit_sw$points %>%  
    dplyr::select(- .data$day_number) %>% 
    dplyr::bind_rows(sinusfit_gw$points %>%  dplyr::select(- .data$day_number)) %>%  
    dplyr::select(- .data$observed, - .data$simulated) %>%  
    tidyr::spread(key = "type", value = "date") %>% 
    dplyr::mutate(traveltime_thermal_days = as.numeric(.data$groundwater - .data$`surface-water`),
                  retardation_factor = retardation_factor,
                  traveltime_hydraulic_days = traveltime_thermal_days / retardation_factor) %>% 
    dplyr::arrange(.data$groundwater) %>%  
    dplyr::select(.data$point_type, 
                  .data$`surface-water`,
                  .data$groundwater, 
                  .data$traveltime_thermal_days,
                  .data$retardation_factor, 
                  .data$traveltime_hydraulic_days)
  
}
