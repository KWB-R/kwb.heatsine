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
# kwb.utils::sourceScripts(
#   dir("R", pattern = "^\\.", recursive = TRUE, full.names = TRUE)
# )

# Load the pipe operator
`%>%` <- magrittr::`%>%`

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  #############################################################################
  ### 1. Data preparation (example data sets for ground- and surface water)
  xls_dir <- "~/../Downloads/kwb-cloud/projects/smart-control/"

  # 1.1 Load temperature from Excel file
  # (file: "Temperatur_in_Br _und_GWM_für_KWB.xlsx" needed!)
  temp <- load_temperature_from_excel(
    dir_path = xls_dir
  )


  ophours <- load_operatinghours_from_excel(dir_path = xls_dir)

  temp_ophours <- temp %>%
    dplyr::left_join(ophours) %>%
    dplyr::filter(!grepl("TEGsee|SPAowa", .data$Name_Messstelle),
                  grepl("^TEG", .data$Name_Messstelle),
                  .data$Betriebsstunden == 24) %>%
    dplyr::filter(.data$PN_Datum >= as.Date("2016-01-01")) #before no full year with data for TEGsee-mikrosiebO


  filter_rename_select <- function(df, name) {
    df %>%
      dplyr::filter(Name_Messstelle == name) %>%
      dplyr::rename(date = PN_Datum, value = Temperatur) %>%
      dplyr::select(date, value)
  }


  data_sw <- filter_rename_select(temp, "TEGsee-mikrosiebO")
  data_gw <- filter_rename_select(temp, "TEG343")


  kwb.utils::createDirectory("csv")

  write_local_csv <- function(df, file_base) {
    readr::write_csv(df, sprintf("inst/extdata/%s.csv", file_base))
  }

  write_local_csv(data_sw, "temperature_surface-water_TEGsee-mikrosieb")
  write_local_csv(data_gw, "temperature_groundwater_TEG343")

  pwells <- unique(temp_ophours$Name_Messstelle)

  sapply(pwells, function(pwell) {
    pwell_file <- sprintf("temperature_groundwater_%s", sub("-?/.*", "", pwell))
    data_pw <- filter_rename_select(temp_ophours, pwell)
    write_local_csv(data_pw, pwell_file)

  })

  ####################################################################
  ### 2. Interactively plot & select data

  load_temp <- function(file_base) {
    kwb.heatsine::load_temperature_from_csv(
      kwb.heatsine::extdata_file(paste0(file_base, ".csv"))
    )
  }

  ### Interactively select time period
  data_sw <- load_temp("temperature_surface-water_TEGsee-mikrosieb")
  data_gw <- load_temp("temperature_groundwater_TEG343")

  # 2.1 Surface water

  ## 2.1.1 Plot time series for whole time period (moveover on data points to select)
  plot_temperature_interactive(data_sw)

  ## 2.1.2 Reduce time period to user input ('date_end' is optional, if not given
  ##       it is set to 'date_start' + 365.25 days)

  data_sw_selected <- kwb.heatsine::select_timeperiod(
    data_sw,
    date_start = "2015-10-10",
    date_end = "2016-10-14"
  )

  data_gw_selected <- kwb.heatsine::select_timeperiod(
    data_gw,
    date_start = "2015-12-28",
    date_end = "2016-12-26"
  )


  kwb.heatsine::plot_temperature_interactive(df = data_sw_selected)
  kwb.heatsine::plot_temperature_interactive(df = data_gw_selected)

  ####################################################################
  ### 3. Optimise sinus fit (surface water and groundwater)
  limits <- c(100,500) # minimum/maximum period length
  tolerance <- 0.001 # the desired accuracy ()
  debug <- TRUE

  do_sinus_optimisation <- function(temp_df) {
    kwb.heatsine::optimise_sinus_variablePeriod(
      temp_df = temp_df,
      opt_limits = limits,
      opt_tolerance = tolerance,
      opt_debug = debug
    )
  }

  sinusfit_sw <- do_sinus_optimisation(temp_df = data_sw_selected)

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

  sinusfit_gw <- do_sinus_optimisation(temp_df = data_gw_selected)

  ####################################################################
  ### 4. Results

  # Generate data frame with simulated and observed values
  predictions <- kwb.heatsine::get_predictions(
    sinusfit_sw, sinusfit_gw, retardation_factor = 1.8
  )

  # Plot results interactively
  kwb.heatsine::plot_prediction_interactive(predictions)

  # Plot residuals interactively (either use binwidth = NULL -> auto-defined or
  # select your own value)
  #plot_residuals_interactive(prediction_df, binwidth = 0.2)

  ####################################################################
  ### 5. Export results

  kwb.utils::createDirectory("csv")

  write_to_csv <- function(df, file_base) readr::write_csv(
    df, path = file.path("csv", paste0(file_base, ".csv"))
  )

  get_residuals <- function(df) data.frame(
    residuals = df$simulated - df$observed
  )

  write_to_csv(predictions$data, "sinus-fit_predictions")
  write_to_csv(predictions$paras, "sinus-fit_parameters")
  write_to_csv(predictions$gof, "sinus-fit_goodness-of-fit")
  write_to_csv(predictions$traveltimes, "sinus-fit_traveltimes")
  write_to_csv(get_residuals(df = predictions$data), "sinus-fit_residuals")

  ## Export plots:
  kwb.utils::createDirectory("plots")

  plot_to_html <- function(p, file_base) {
    htmlwidgets::saveWidget(p, paste0(file_base, ".html"))
  }

  withr::with_dir(new = "plots", code = {

    plot_to_html(
      plot_temperature_interactive(data_sw),
      "temperature_surface-water_time-series_full"
    )

    plot_to_html(
      plot_temperature_interactive(data_sw_selected),
      "temperature_surface-water_time-series_selected"
    )

    plot_to_html(
      plot_temperature_interactive(data_gw),
      "temperature_groundwater_time-series_full"
    )

    plot_to_html(
      plot_temperature_interactive(data_gw_selected),
      "temperature_groundwater_time-series_selected"
    )

    plot_to_html(
      plot_prediction_interactive(predictions),
      "temperature_prediction"
    )

    # plot_to_html(
    #   plot_residuals_interactive(prediction_df, binwidth = 0.5),
    #   "temperature_prediction_residuals"
    # )
  })
}

### Test Monte Carlo
if (FALSE)
{
  res_sw <- run_montecarlo(sinus_fit_list = sinusfit_sw, nMC = 1000)
  res_gw <- run_montecarlo(sinus_fit_list = sinusfit_gw, nMC = 1000)

  g <- ggplot2::ggplot(res_sw[1:1000, ], ggplot2::aes(
    x = day_number,
    y = y_r_plus_sigma_r,
    col = run_id
  )) +
    # ggplot2::facet_wrap(~ run_id) +
    ggplot2::geom_line()

  plotly::ggplotly(g)

  get_range_per_run <- function(df) {
    df %>%
      dplyr::select(.data$run_id, .data$day_number, .data$y_r_plus_sigma_r) %>%
      dplyr::group_by(.data$run_id) %>%
      dplyr::summarise(
        min = which.min(.data$y_r_plus_sigma_r),
        max = which.max(.data$y_r_plus_sigma_r)
      )
  }

  mr_sw <- get_range_per_run(res_sw)
  mr_gw <- get_range_per_run(res_gw)

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
