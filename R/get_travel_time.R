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
#' @importFrom dplyr enquo bind_rows
#'
get_travel_time <- function(sinusfit_sw, sinusfit_gw, retardation_factor = 1.8) {

  label_sw <- sinusfit_sw$metadata$label
  label_gw <- sinusfit_gw$metadata$label

  sinusfit_sw$points %>%
    dplyr::select(-.data$day_number) %>%
    dplyr::bind_rows(sinusfit_gw$points %>% dplyr::select(-.data$day_number)) %>%
    dplyr::select(-.data$observed, -.data$simulated) %>%
    tidyr::spread(key = "label", value = "date") %>%
    dplyr::mutate(
      traveltime_thermal_days = as.numeric(.data[[label_gw]] - .data[[label_sw]]),
      retardation_factor = retardation_factor,
      traveltime_hydraulic_days = .data$traveltime_thermal_days / retardation_factor
    ) %>%
    dplyr::arrange(.data[[label_gw]]) %>%
    dplyr::select(
      .data$point_type,
      .data[[label_sw]],
      .data[[label_gw]],
      .data$traveltime_thermal_days,
      .data$retardation_factor,
      .data$traveltime_hydraulic_days
    )
}
