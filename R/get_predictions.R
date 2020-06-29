
#' Get Predictions
#'
#' @param sinusfit_sw as retrieved by \code{\link{optimise_sinus_variablePeriod}} with surface water temperature data
#' @param sinusfit_gw as retrieved by \code{\link{optimise_sinus_variablePeriod}} with groundwater temperature data
#' @param retardation_factor hydraulic retardation factor (default: 2)
#'
#' @return list with sim/observation data ("data") fit parameters ("paras"), goodness-of-fit values ("gof")
#' traveltimes ("traveltimes") and special (min, max, turning) points ("points")
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom stats predict
#'
get_predictions <- function(sinusfit_sw, sinusfit_gw, retardation_factor = 2)
{
  ### Use "prediction" intervall (uncertainty of single value)
  ### Reference: http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/

  rename_fit_to_sim <- function(df) dplyr::rename(df, simulated = "fit")

  get_prediction <- function(sinusfit) {

    period_length <- sinusfit$paras$period_length
    day_numbers <- seq_len(period_length) - 1L

    data.frame(
      type = sinusfit$metadata$type,
      monitoring_id = sinusfit$metadata$monitoring_id,
      label = sinusfit$metadata$label,
      date = min(sinusfit$data$date, na.rm = TRUE) + day_numbers,
      simulate(
        model = sinusfit$lm_object,
        day_number = day_numbers,
        period_length = period_length,
        interval = "prediction"
      )
    ) %>%
      rename_fit_to_sim()
  }

  pred_sw <- get_prediction(sinusfit = sinusfit_sw)
  pred_gw <- get_prediction(sinusfit = sinusfit_gw)



   # predict_confidence <- function(sinusfit) {
  #   predict(sinusfit$lm_object, interval = "confidence") %>%
  #     as.data.frame() %>%
  #     rename_fit_to_sim()
  # }
  #
  # ci_sw <- predict_confidence(sinusfit = sinusfit_sw)
  # ci_gw <- predict_confidence(sinusfit = sinusfit_gw)

  rename_right_join <- function(sinusfit, pred) {
    sinusfit$data %>%
      dplyr::rename(observed = .data$value) %>%
      dplyr::right_join(pred, by = c("type", "monitoring_id", "label", "date"))
  }

  dat <- dplyr::bind_rows(
    rename_right_join(sinusfit = sinusfit_sw, pred = pred_sw),
    rename_right_join(sinusfit = sinusfit_gw, pred = pred_gw)
  ) %>%
    dplyr::rename(
      simulated_pi_lower = .data$lwr,
      simulated_pi_upper = .data$upr
    )

  bind_rows_with_type <- function(sw, gw) {
    dplyr::bind_rows(.id = "type", list(
      `surface-water` = sw,
      groundwater = gw
    ))
  }

  list(
    data = dat,
    paras = bind_rows_with_type(
      sw = sinusfit_sw$paras,
      gw = sinusfit_gw$paras
    ),
    gof = bind_rows_with_type(
      sw = sinusfit_sw$gof,
      gw = sinusfit_gw$gof
    ),
    traveltimes = get_travel_time(
      sinusfit_sw,
      sinusfit_gw,
      retardation_factor = retardation_factor
    ),
    points =  dplyr::bind_rows(sinusfit_sw$points, sinusfit_gw$points)
  )
}
