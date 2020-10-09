
#' Wrapper function for sinus optimisation
#'
#' @param data_sw_selected data.frame with daily data temperature data of surface water monitoring point
#' with columns "date" (format: "YYYY-MM-DD") and "value" (format: double, temperature in degree Celsius)
#' for selected time period
#' @param data_gw_selected data.frame with daily data temperature data of groundwater monitoring point
#' with columns "date" (format: "YYYY-MM-DD") and "value" (format: double, temperature in degree Celsius)
#' for selected time period
#' @param retardation_factor hydraulic retardation factor (default: 2)
#' @param sw_monitoring_id optional label for surface water monitoring id (default: "surface-water monitoring point"
#' or attr(data_sw_selected, "monitoring_id") if data imported with  \code{\link{load_temperature_from_csv}}), otherwise
#' can be any user-defined character string to be used as label for the monitoring point
#' @param gw_monitoring_id optional label for groundwater monitoring id (default: "surface-water monitoring point"
#' or attr(data_sw_selected, "monitoring_id") if data imported with  \code{\link{load_temperature_from_csv}}), otherwise
#' can be any user-defined character string to be used as label for the monitoring point
#' @param limits minimum/maximum period length for sinus optimisation in days (default: c(100, 500))
#' @param tolerance the desired accuracy (default: 0.001
#' @param debug show debug messages (default: FALSE)
#' @return list with sim/observation data ("data") fit parameters ("paras"), goodness-of-fit values ("gof")
#' traveltimes ("traveltimes") and special (min, max, turning) points ("points") as returned by \code{\link{get_predictions}}
#' @export
#' @examples
#' load_temp <- function(base_name) {
#'  kwb.heatsine::load_temperature_from_csv(
#'  kwb.heatsine::extdata_file(base_name)
#'  )
#' }
#'
#' data_sw <- load_temp("temperature_surface-water_TEGsee-mikrosieb.csv")
#' data_gw <- load_temp("temperature_groundwater_TEG343.csv")
#'
#' data_sw_selected <- kwb.heatsine::select_timeperiod(
#'  data_sw,
#'  date_start = "2015-10-10",
#'  date_end = "2016-10-14"
#' )
#'
#' data_gw_selected <- kwb.heatsine::select_timeperiod(
#'  data_gw,
#'  date_start = "2015-12-28",
#'  date_end = "2016-12-26"
#' )
#'
#' kwb.heatsine::run_optimisation(data_sw_selected = data_sw_selected,
#' data_gw_selected = data_gw_selected,
#' retardation_factor = 1.8,
#' sw_monitoring_id = attr(data_sw_selected, "monitoring_id"),
#' gw_monitoring_id = attr(data_gw_selected, "monitoring_id"),
#' limits = c(100, 500),
#' tolerance = 0.001,
#' debug = FALSE)

run_optimisation <- function (data_sw_selected,
                              data_gw_selected,
                              retardation_factor = 2,
                              sw_monitoring_id = ifelse(!is.null(attr(data_sw_selected, "monitoring_id")),
                                                        attr(data_sw_selected, "monitoring_id"),
                                                        "surface-water monitoring point"),
                              gw_monitoring_id = ifelse(!is.null(attr(data_gw_selected, "monitoring_id")),
                                                        attr(data_gw_selected, "monitoring_id"),
                                                        "groundwater monitoring point"),
                              limits = c(100, 500),
                              tolerance = 0.001,
                              debug = FALSE
                           ) {


set_label <- function(type, monitoring_id) {
  sprintf("%s (%s)", type, monitoring_id)
}


data_sw_selected <- structure(data_sw_selected,
                              type = "surface-water",
                              monitoring_id = sw_monitoring_id,
                              label = set_label("surface-water", sw_monitoring_id)
                              )


data_gw_selected <- structure(data_gw_selected,
                              type = "groundwater",
                              monitoring_id = gw_monitoring_id,
                              label = set_label("groundwater", gw_monitoring_id)
                              )

# Helper function to do the sinus optimisation
do_sinus_optimisation <- function(temp_df) {
  optimise_sinus_variablePeriod(
    temp_df = temp_df,
    opt_limits = limits,
    opt_tolerance = tolerance,
    opt_debug = debug
  )
}


sinusfit_sw <- do_sinus_optimisation(temp_df = data_sw_selected)

sinusfit_gw <- do_sinus_optimisation(temp_df = data_gw_selected)

# Generate data frame with simulated and observed values
get_predictions(
  sinusfit_sw = sinusfit_sw,
  sinusfit_gw = sinusfit_gw,
  retardation_factor = retardation_factor
)

}
