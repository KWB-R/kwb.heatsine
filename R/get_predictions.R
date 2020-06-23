
#' Get Predictions
#'
#' @param sinusfit_sw as retrieved by \code{\link{optimise_sinus_variablePeriod}} with surface water temperature data
#' @param sinusfit_gw as retrieved by \code{\link{optimise_sinus_variablePeriod}} with groundwater temperature data
#' @param retardation_factor hydraulic retardation factor (default: 2)
#'
#' @return list with sim/observation data ("data") fit parameters ("paras"), goodness-of-fit values ("gof")
#' and traveltimes ("traveltimes")
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom stats predict
#'
get_predictions <- function(sinusfit_sw,
                            sinusfit_gw,
                            retardation_factor = 2) {


  ### Use "prediction" intervall (uncertainty of single value)
  ### Reference: http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/

  day_numbers_sw <- seq_len(sinusfit_sw$paras$period_length) - 1
  dates_sw <- min(sinusfit_sw$data$date, na.rm = TRUE) + day_numbers_sw

  pred_sw <- dplyr::bind_cols(
    tibble::tibble(
      type = "surface-water",
      date = dates_sw
    ),
    stats::predict(sinusfit_sw$lm_object,
      newdata = data.frame(day_number = day_numbers_sw),
      interval = "prediction"
    ) %>%
      as.data.frame()
  ) %>%
    dplyr::rename(simulated = "fit") %>%
    dplyr::mutate(date = dates_sw)

  day_numbers_gw <- seq_len(sinusfit_gw$paras$period_length) - 1
  dates_gw <- min(sinusfit_gw$data$date, na.rm = TRUE) + day_numbers_gw

  pred_gw <- dplyr::bind_cols(
    tibble::tibble(
      type = "groundwater",
      date = dates_gw
    ),
    stats::predict(sinusfit_gw$lm_object,
      newdata = data.frame(day_number = day_numbers_gw),
      interval = "prediction"
    ) %>%
      as.data.frame()
  ) %>%
    dplyr::rename(simulated = "fit")

  # ci_sw <- predict(sinusfit_sw$lm_object, interval = "confidence") %>%
  #   as.data.frame() %>%
  #   dplyr::rename(simulated = "fit")
  #
  #
  # ci_gw <- predict(sinusfit_gw$lm_object, interval = "confidence") %>%
  #   as.data.frame() %>%
  #   dplyr::rename(simulated = "fit")

  dat <- sinusfit_sw$data %>%
    dplyr::rename(observed = .data$value) %>%
    dplyr::right_join(pred_sw, by = c("type", "date")) %>%
    dplyr::bind_rows(sinusfit_gw$data %>%
      dplyr::rename(observed = .data$value) %>%
      dplyr::right_join(pred_gw, by = c("type", "date"))) %>%
    dplyr::rename(
      simulated_pi_lower = .data$lwr,
      simulated_pi_upper = .data$upr
    )

  res_opti_paras <- dplyr::bind_rows(list(
    `surface-water` = sinusfit_sw$paras,
    groundwater = sinusfit_gw$paras
  ),
  .id = "type"
  )



  res_opti_gof <- dplyr::bind_rows(list(
    `surface-water` = sinusfit_sw$gof,
    groundwater = sinusfit_gw$gof
  ),
  .id = "type"
  )

  res_opti_traveltimes <- get_travel_time(sinusfit_sw,
    sinusfit_gw,
    retardation_factor = retardation_factor
  )

  list(
    data = dat,
    paras = res_opti_paras,
    gof = res_opti_gof,
    traveltimes = res_opti_traveltimes
  )
}
