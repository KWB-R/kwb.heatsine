mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE){
  dat <- data.frame(y = mean(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)
}

get_tidy_traveltimes <- function(traveltimes) {
  

tidyr::gather(traveltimes, 
              key = "type", 
              value = "date", 
              - .data$point_type, 
              - .data$traveltime_thermal_days, 
              - .data$retardation_factor, 
              - .data$traveltime_hydraulic_days)

}


plot_prediction_interactive <- function(predictions) {
  
  traveltimes_tidy <- get_tidy_traveltimes(predictions$traveltimes) %>% 
    dplyr::mutate(label = dplyr::if_else(.data$type == "groundwater", 
                                         sprintf("%s (%s): traveltime_thermal: %3.1f days, traveltime_hydraulic: %3.1f days",
                                                 .data$point_type,
                                                 .data$date,
                                                 .data$traveltime_thermal_days,
                                                 .data$traveltime_hydraulic_days),
                                         sprintf("%s (%s)",
                                                 .data$point_type,
                                                 .data$date)))
  
  g1 <- predictions$data  %>% 
    tidyr::gather(key = "temperature", value = "value", -type , -date, 
                  - simulated_pi_lower, - simulated_pi_upper) %>%  
    dplyr::mutate(simulated_pi_lower = ifelse(.data$temperature == "observed", 
                                              NA_real_, simulated_pi_lower),
                  simulated_pi_upper = ifelse(.data$temperature == "observed", 
                                              NA_real_, simulated_pi_upper)) %>% 
    ggplot2::ggplot(ggplot2::aes_string(x = "date", y = "value", col = "temperature")) + 
    ggplot2::facet_wrap(~ forcats::fct_rev(type), ncol = 1) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(data = traveltimes_tidy, mapping = ggplot2::aes(xintercept = as.numeric(.data$date),
                                                                        col = .data$point_type), 
                        alpha = 0.5,
                        show.legend = FALSE) +
    # ggplot2::geom_text(data=traveltimes_tidy, mapping= ggplot2::aes(x=.data$date, 
    #                                                                 y=0, 
    #                                                                 label=.data$label), 
    #                    size=4, 
    #                    angle=90, 
    #                    vjust=-0.4, 
    #                    hjust=0) +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "simulated_pi_lower", 
                                             ymax = "simulated_pi_upper"), 
                         linetype=2, 
                         alpha=0.1,
                         show.legend = FALSE) +
    ggplot2::ylab(label = "temperature (\u00B0 C)") +
    ggplot2::scale_x_date(date_labels = "%Y/%m/%d") + 
  # ggplot2::scale_fill_manual("", labels = c("Prediction interval"), values = c("grey")) +
    ggplot2::theme_bw()
  
  plotly::ggplotly(g1)
}
