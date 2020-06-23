#' Optimise Sinus Fit for Fixed Period
#'
#' @param df data frame with temperature data and columns "type" (i.e. "groundwater", "surface-water"), "date" and "value"
#' @param period_length period length (default: 365.25)
#' @return list with fit parameters ("paras"), goodness-of-fit values ("gof"), special points, i.e. min/max/turning-points
#' ("points), fit model ("lm_model") and input data ("data")
#' @references  https://stats.stackexchange.com/questions/77543/how-do-i-get-the-amplitude-and-phase-for-sine-wave-from-lm-summary
#' @export
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble tibble
#' @importFrom lubridate ymd
#' @importFrom dplyr rename select mutate case_when bind_rows left_join right_join arrange
#' @importFrom stats coef lm
#' @importFrom hydroGOF gof
optimise_sinus_fixedPeriod <- function(df,
                                       period_length = 365.25) {
  df <- df[order(df$date), ]

  dates_all <- tibble::tibble(
    type = df$type[1],
    date = seq(lubridate::ymd(min(df$date, na.rm = TRUE)),
      lubridate::ymd(max(df$date, na.rm = TRUE)),
      by = "days"
    )
  )

  df <- df %>%
    dplyr::right_join(dates_all, by = c("type", "date")) %>%
    dplyr::arrange(.data$date)

  df$day_number <- as.numeric(df$date - df$date[1])


  fit.lm2 <- stats::lm(value ~ sin(2 * pi * day_number / period_length) + cos(2 * pi * day_number / period_length),
    data = df
  )
  # summary(fit.lm2)
  gof <- tibble::as_tibble(t(hydroGOF::gof(
    sim = fit.lm2$fitted.values,
    obs = fit.lm2$model$value
  ))) %>%
    janitor::clean_names()



  paras <- tibble::tibble(
    period_length = period_length,
    alpha = stats::coef(fit.lm2)[2],
    beta = stats::coef(fit.lm2)[3],
    y0 = stats::coef(fit.lm2)[1],
    a = sqrt(alpha^2 + beta^2),
    x0 = atan2(beta, alpha)
  )



  is_max <- which.max(fit.lm2$fitted.values)
  is_min <- which.min(fit.lm2$fitted.values)

  date_max <- df$date[as.numeric(names(is_max))]
  date_min <- df$date[as.numeric(names(is_min))]


  extrema <- tibble::tibble(
    type = df$type[1],
    date = c(date_max, date_min),
    day_number = as.integer(df$day_number[1] + date - df$date[1]),
    simulated = predict(fit.lm2, newdata = tibble::tibble(day_number = .data$day_number)),
    point_type = c("max", "min")
  ) %>%
    dplyr::left_join(tibble::as_tibble(df[, c("day_number", "value")]), by = "day_number") %>%
    dplyr::rename(observed = .data$value) %>%
    dplyr::select(
      .data$type,
      .data$day_number,
      .data$date,
      .data$observed,
      .data$simulated,
      .data$point_type
    )

  quarter_period <- period_length / 4

  turning_points <- tibble::tibble(
    type = df$type[1],
    point_type = sprintf("turning-point_%d", 1:3),
    date = dplyr::case_when(
      date_max > date_min ~ c(date_min - quarter_period, date_min + quarter_period, date_max + quarter_period),
      date_min > date_max ~ c(date_max - quarter_period, date_max + quarter_period, date_min + quarter_period)
    ) %>%
      as.Date(),
    day_number = as.integer(df$day_number[1] + .data$date - df$date[1]),
    simulated = predict(fit.lm2, newdata = tibble::tibble(day_number = .data$day_number))
  ) %>%
    dplyr::left_join(tibble::as_tibble(df[, c("day_number", "value")]), by = "day_number") %>%
    dplyr::rename(observed = .data$value) %>%
    dplyr::select(
      .data$type,
      .data$day_number,
      .data$date,
      .data$observed,
      .data$simulated,
      .data$point_type
    )


  points <- dplyr::bind_rows(turning_points, extrema) %>%
    dplyr::arrange(.data$date)

  list(
    paras = paras,
    gof = gof,
    points = points,
    lm_object = fit.lm2,
    data = df
  )
}

#' Optimise Sinus Fit Function
#'
#' @param period period length
#' @param df data frame with temperature data and columns "type" (i.e. "groundwater", "surface-water"), "date" and "value"
#' @param opt_criteria (default: "rmse"), for other options check: ?hydroGOF::gof
#' @param debug show debug messages (default: TRUE)
#' @return scalar with optimisation result
#' @export
#' @importFrom dplyr pull
#'
opt_func <- function(period,
                     df,
                     opt_criteria = "rmse",
                     debug = TRUE) {
  f1 <- optimise_sinus_fixedPeriod(df, period_length = period)

  if (debug) {
    cat(sprintf(
      "Ran opt_func with period length %3.2f days: %2.2f '%s' for Temp\n",
      period,
      f1$gof[opt_criteria],
      opt_criteria
    ))
  }

  dplyr::pull(f1$gof, opt_criteria)
}

#' Optimise Sinus Fit for Variable Period
#'
#' @param temp_df data frame with temperature data and columns "type" (i.e. "groundwater", "surface-water"), "date" and "value"
#' @param optFunc optimisation function (default: kwb.heatsine::optFunc())
#' @param opt_limits optimisation limits for "period_length" (default: c(100,500))
#' @param opt_tolerance (default: 0.001)
#' @param opt_debug  show debug information (default: FALSE)
#' @return list with fit parameters ("paras"), goodness-of-fit values ("gof"), special points, i.e. min/max/turning-points
#' ("points), fit model ("lm_model") and input data ("data")
#' @references  https://stats.stackexchange.com/questions/77543/how-do-i-get-the-amplitude-and-phase-for-sine-wave-from-lm-summary
#' @importFrom stats optimise
#'
optimise_sinus_variablePeriod <- function(
                                          temp_df,
                                          optFunc = opt_func,
                                          opt_limits = c(100, 500),
                                          opt_tolerance = 0.001,
                                          opt_debug = FALSE) {
  opt <- optimise(
    f = optFunc,
    interval = opt_limits,
    df = temp_df,
    tol = opt_tolerance,
    debug = opt_debug
  )

  optimise_sinus_fixedPeriod(
    df = temp_df,
    period_length = opt$minimum
  )
}
