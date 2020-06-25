#' Helper function: get traveltime
#'
#' @param sinusfit_sw as retrieved by \code{\link{optimise_sinus_variablePeriod}} with surface water temperature data
#' @param sinusfit_gw as retrieved by \code{\link{optimise_sinus_variablePeriod}} with groundwater temperature data
#' @param retardation_factor hydraulic retardation factor (default: 2)
#'
#' @return data frame with travel times for min/max and turning points
#' @export
#'
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#'
get_travel_time <- function(sinusfit_sw, sinusfit_gw, retardation_factor = 1.8) {
  sinusfit_sw$points %>%
    dplyr::select(-.data$day_number) %>%
    dplyr::bind_rows(sinusfit_gw$points %>% dplyr::select(-.data$day_number)) %>%
    dplyr::select(-.data$observed, -.data$simulated) %>%
    tidyr::spread(key = "label", value = "date") %>%
    dplyr::mutate(
      traveltime_thermal_days = as.numeric(.data$groundwater - .data$`surface-water`),
      retardation_factor = retardation_factor,
      traveltime_hydraulic_days = .data$traveltime_thermal_days / retardation_factor
    ) %>%
    dplyr::arrange(.data$groundwater) %>%
    dplyr::select(
      .data$point_type,
      .data$`surface-water`,
      .data$groundwater,
      .data$traveltime_thermal_days,
      .data$retardation_factor,
      .data$traveltime_hydraulic_days
    )
}
