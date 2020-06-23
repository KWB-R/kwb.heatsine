# Instructions -----------------------------------------------------------------
#
# - Source the whole script to load the functions defined below
# - Manually go through the instructions in the MAIN section
#

# Install R packages -----------------------------------------------------------

#Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
#remotes::install_github("guiastrennec/ggplus")

# Load R functions from other scripts ------------------------------------------
# kwb.utils::sourceScripts(
#   dir("functions_temperature-fitting", full.names = TRUE)
# )
kwb.utils::sourceScripts(
  dir("functions_temperature-fitting_simple-script", full.names = TRUE)
)
# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  
  `%>%` <- magrittr::`%>%`
  #############################################################################
  ### 1. Data preparation (example data sets for ground- and surface water)
  
  # 1.1 Load temperature from Excel file (file: "Temperatur_in_Br _und_GWM_für_KWB.xlsx" needed!)
  temp <- load_temperature_from_excel(dir_path = "~/../Downloads/kwb-cloud/projects/smart-control")
  
  
  data_sw <- temp %>%  
    dplyr::filter(Name_Messstelle == "TEGsee-mikrosiebO") %>%  
    dplyr::rename(date = PN_Datum, value = Temperatur) %>% 
    dplyr::mutate(type = "surface-water") %>%  
    dplyr::select(type, date, value)
  
  data_gw <- temp %>%  
    dplyr::filter(Name_Messstelle == "TEG343") %>%  
    dplyr::rename(date = PN_Datum, value = Temperatur) %>% 
    dplyr::mutate(type = "groundwater") %>%  
    dplyr::select(type, date, value) 
  
  kwb.utils::createDirectory("csv")
  
  readr::write_csv(data_sw, path = "csv/temperature_surface-water.csv")  
  readr::write_csv(data_gw, path = "csv/temperature_groundwater.csv")
  
  ####################################################################
  ### 2. Interactively plot & select data
  
  ### Interactively select time period
  data_sw <- readr::read_csv(file = "csv/temperature_surface-water.csv", 
                             col_types = "cDd")  
  data_gw <- readr::read_csv(file = "csv/temperature_groundwater.csv", 
                             col_types = "cDd")
  
  # 2.1 Surface water
  
  ## 2.1.1 Plot time series for whole time period (moveover on data points to select)
  plot_temperature_interactive(data_sw) 
  
  ## 2.1.2 Reduce time period to user input ('date_end' is optional, if not given 
  ##       it is set to 'date_start' + 365.25 days)
  
  data_sw_selected <- select_timeperiod(data_sw, 
                                        date_start = "2015-10-10",
                                        date_end = "2016-10-14")
  
  plot_temperature_interactive(df = data_sw_selected)
  
  
  plot_temperature_interactive(data_gw)
  
  data_gw_selected <- select_timeperiod(data_gw, 
                                        date_start = "2015-12-28",
                                        date_end = "2016-12-26")
  
  plot_temperature_interactive(df = data_gw_selected)
  
  
  ####################################################################
  ### 3. Optimise sinus fit (surface water and groundwater)  
  limits <- c(100,500) # minimum/maximum period length
  tolerance <- 0.001 # the desired accuracy ()
  debug <- TRUE
  sinusfit_sw <- optimise_sinus_variablePeriod(temp_df = data_sw_selected, 
                                               opt_limits = limits, 
                                               opt_tolerance = tolerance,
                                               opt_debug = debug)
  
  
  
  
  # Check results  
  # y0 <- sinusfit_sw$paras$y0
  # a <- sinusfit_sw$paras$a
  # x0 <- sinusfit_sw$paras$x0
  # period_length <- sinusfit_sw$paras$period_length
  # x <- seq(0, to = period_length, by = 1)
  # y0 + a * sin(2*pi/period_length * x + x0)
  # a * sin(0.5*pi/period_length + x0)
  # y0 + a * sin(2*pi/period_length * x - x0)
  # plot(x, y0 + a * sin(2*pi/period_length * x + x0))
  
  # # sim -----------------------------------------------------------------
  # sim <- function (yo, A, xo, w, x)
  # {
  #   yo + A * sin(pi * (x - xo) / w)
  # }
  # 
  
  sinusfit_gw <- optimise_sinus_variablePeriod(temp_df = data_gw_selected, 
                                               opt_limits = limits, 
                                               opt_tolerance = tolerance,
                                               opt_debug = debug)
  
  
  ####################################################################
  ### 4. Results
  
  # Generate data frame with simulated and observed values  
  predictions <- get_predictions(sinusfit_sw, 
                                 sinusfit_gw, 
                                 data_sw_selected, 
                                 data_gw_selected, 
                                 retardation_factor = 1.8)
  
  
  # Plot results interactively
  plot_prediction_interactive(predictions) 
  
  # Plot residuals interactively (either use binwidth = NULL -> auto-defined or 
  # select your own value)
  plot_residuals_interactive(prediction_df, binwidth = 0.2) 
  
  

  
  ####################################################################
  ### 5. Export results
  
  kwb.utils::createDirectory("csv")
  readr::write_csv(prediction_df, 
                   path = "csv/sinus-fit_predictions.csv")
  readr::write_csv(res_opti_paras, 
                   path = "csv/sinus-fit_parameters.csv") 
  readr::write_csv(res_opti_gof, 
                   path = "csv/sinus-fit_goodness-of-fit.csv") 
  readr::write_csv(res_opti_traveltimes, 
                   path = "csv/sinus-fit_traveltimes.csv")
  readr::write_csv(data.frame(residuals = prediction_df$simulated-prediction_df$observed), 
                   "csv/sinus-fit_residuals.csv")
  
  
  ## Export plots: 
  kwb.utils::createDirectory("plots")
  withr::with_dir(new = "plots", code = {
    plot_temperature_interactive(data_sw) %>%
      htmlwidgets::saveWidget("temperature_surface-water_time-series_full.html")
    
    plot_temperature_interactive(df = data_sw_selected) %>%
      htmlwidgets::saveWidget("temperature_surface-water_time-series_selected.html")
    
    plot_temperature_interactive(data_gw) %>%
      htmlwidgets::saveWidget("temperature_groundwater_time-series_full.html")
    plot_temperature_interactive(df = data_gw_selected) %>%
      htmlwidgets::saveWidget("temperature_groundwater_time-series_selected.html")
    
    plot_prediction_interactive(prediction_df) %>%
      htmlwidgets::saveWidget("temperature_prediction.html")
    
    plot_residuals_interactive(prediction_df, binwidth = 0.5) %>% 
      htmlwidgets::saveWidget("temperature_prediction_residuals.html")  
  }
  )
  
  
}


### Test Monte Carlo
if(FALSE) {
  res_sw <- run_montecarlo(sinus_fit_list = sinusfit_sw, nMC = 1000)
  res_gw <- run_montecarlo(sinus_fit_list = sinusfit_gw, nMC = 1000)
  
  
  g <- ggplot2::ggplot(res_sw[1:1000, ], ggplot2::aes(x=day_number, 
                                                      y = y_r_plus_sigma_r,
                                                      col = run_id)) +
    # ggplot2::facet_wrap(~ run_id) +
    ggplot2::geom_line()
  
  plotly::ggplotly(g)
  
  mr_sw <- res_sw %>%
    dplyr::select(run_id, day_number, y_r_plus_sigma_r) %>% 
    dplyr::group_by(run_id) %>% 
    dplyr::summarise(min = which.min(y_r_plus_sigma_r),
                     max = which.max(y_r_plus_sigma_r))
  
  mr_gw <- res_gw %>%
    dplyr::select(run_id, day_number, y_r_plus_sigma_r) %>% 
    dplyr::group_by(run_id) %>% 
    dplyr::summarise(min = which.min(y_r_plus_sigma_r),
                     max = which.max(y_r_plus_sigma_r))
  
  hist(mr_sw$min)
  
  # get mean and uncertainty
  sw_min_mean <- mean(mr_sw$min)
  sw_min_uncertainty <- 2 * sd(mr_sw$min)
  
  gw_min_mean <- mean(mr_gw$min)
  gw_min_uncertainty <- 2 * sd(mr_gw$min)
  
  ### this part of the script
  ### can be used to calculated the uncertainty
  ### of the travel time
  
  myear1 <- 60
  uyear1 <- 20
  
  myear2 <- 110
  uyear2 <- 23
  
  mtravel <- gw_min_mean - sw_min_mean
  utravel <- sqrt(gw_min_uncertainty^2 + sw_min_uncertainty^2)
  mtravel
  utravel
  hist(mr_sw$max)
}
