#' Optimise Sinus Fit for Fixed Period
#'
#' @param df data frame with temperature data and columns "date" (YYYY-MM-DD)
#'   and "value"
#' @param period_length period length (default: 365.25)
#' @return list with fit parameters ("paras"), goodness-of-fit values ("gof"),
#'   special points, i.e. min/max/turning-points ("points), fit model
#'   ("lm_model") and input data ("data")
#' @references  https://stats.stackexchange.com/questions/77543/how-do-i-get-the-amplitude-and-phase-for-sine-wave-from-lm-summary
#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom lubridate ymd
#' @importFrom dplyr rename select mutate case_when bind_rows left_join
#' @importFrom dplyr right_join arrange
#' @importFrom stats coef lm
#' @importFrom hydroGOF gof
optimise_sinus_fixedPeriod <- function(df, period_length = 365.25)
{
  metadata <- attributes(df)

  backbone <- tibble::tibble(
    type = metadata$type,
    monitoring_id = metadata$monitoring_id,
    label = metadata$label
  )

  df <- backbone %>%
    dplyr::bind_cols(df) %>%
    dplyr::arrange(.data$date)

  day_seq_from_range <- function(rng) seq(
    lubridate::ymd(rng[1L]),
    lubridate::ymd(rng[2L]),
    by = "days"
  )

  dates_all <- backbone %>%
    dplyr::bind_cols(tibble::tibble(
      date = day_seq_from_range(rng = range(df$date, na.rm = TRUE))
    ))

  df <- df %>%
    dplyr::right_join(dates_all, by = c(names(backbone), "date")) %>%
    dplyr::arrange(.data$date)

  df$day_number <- as.numeric(df$date - df$date[1])

  fit.lm2 <- stats::lm(
    value ~ sin(phi) + cos(phi),
    data = add_phi(df, period_length, dbg = FALSE)
  )

  # summary(fit.lm2)

  gof <- tibble::as_tibble(t(hydroGOF::gof(
    sim = fit.lm2$fitted.values,
    obs = fit.lm2$model$value
  )))

  coeffs <- stats::coef(fit.lm2)

  paras <- tibble::tibble(
    period_length = period_length,
    alpha = coeffs[2L], # sin(phi)
    beta = coeffs[3L], # cos(phi)
    y0 = coeffs[1L], # Intercept
    a = sqrt(alpha^2 + beta^2),
    x0 = atan2(beta, alpha)
  )

  get_extreme <- function(FUN) {
    df$date[as.numeric(names(FUN(fit.lm2$fitted.values)))]
  }

  date_max <- get_extreme(FUN = which.max)
  date_min <- get_extreme(FUN = which.min)

  left_join_day_number_value <- function(df_left, df) {
    df_left %>%
      dplyr::left_join(
        tibble::as_tibble(df[, c("day_number", "value")]),
        by = "day_number"
      ) %>%
      dplyr::rename(observed = .data$value) %>%
      dplyr::select(
        .data$label,
        .data$day_number,
        .data$date,
        .data$observed,
        .data$simulated,
        .data$point_type
      )
  }

  extrema <- tibble::tibble(
    label = metadata$label,
    date = c(date_max, date_min),
    day_number = as.integer(df$day_number[1] + date - df$date[1]),
    simulated = simulate(fit.lm2, day_number, period_length),
    point_type = c("max", "min")
  ) %>%
    left_join_day_number_value(df)

  quarter_period <- period_length / 4

  turning_points <- tibble::tibble(
    label = metadata$label,
    point_type = sprintf("turning-point_%d", 1:3),
    date = dplyr::case_when(
      date_max > date_min ~ c(date_min - quarter_period, date_min + quarter_period, date_max + quarter_period),
      date_min > date_max ~ c(date_max - quarter_period, date_max + quarter_period, date_min + quarter_period)
    ) %>%
      as.Date(),
    day_number = as.integer(df$day_number[1] + .data$date - df$date[1]),
    simulated = simulate(fit.lm2, day_number, period_length)
  ) %>%
    left_join_day_number_value(df)

  points <- dplyr::bind_rows(turning_points, extrema) %>%
    dplyr::arrange(.data$date)

  df <- df %>% dplyr::select(- .data$day_number)

  list(
    paras = paras,
    gof = gof,
    points = points,
    lm_object = fit.lm2,
    data = df,
    metadata = metadata
  )
}

#' Optimise Sinus Fit Function
#'
#' @param period period length
#' @param df data frame with temperature data and columns "date" (YYYY-MM-DD)
#'   and "value"
#' @param opt_criteria (default: "RMSE"), for other options check:
#'   ?hydroGOF::gof
#' @param debug show debug messages (default: TRUE)
#' @return scalar with optimisation result
#' @export
#' @importFrom dplyr pull
#'
opt_func <- function(period, df, opt_criteria = "RMSE", debug = TRUE)
{
  f1 <- optimise_sinus_fixedPeriod(df, period_length = period)

  kwb.utils::catIf(debug, sprintf(
    "Ran opt_func with period length %3.2f days: %2.2f '%s' for Temp\n",
    period,
    f1$gof[opt_criteria],
    opt_criteria
  ))

  dplyr::pull(f1$gof, opt_criteria)
}

#' Optimise Sinus Fit for Variable Period
#'
#' @param temp_df data frame with temperature data and columns "date"
#'   (YYYY-MM-DD) and "value"
#' @param optFunc optimisation function (default: \code{\link{opt_func}} )
#' @param opt_limits optimisation limits for "period_length" (default:
#'   c(100,500))
#' @param opt_tolerance (default: 0.001)
#' @param opt_debug  show debug information (default: FALSE)
#' @return list with fit parameters ("paras"), goodness-of-fit values ("gof"),
#'   special points, i.e. min/max/turning-points ("points), fit model
#'   ("lm_model") and input data ("data")
#' @references
#'   https://stats.stackexchange.com/questions/77543/how-do-i-get-the-amplitude-and-phase-for-sine-wave-from-lm-summary
#'
#' @importFrom stats optimise
#' @export
#'
optimise_sinus_variablePeriod <- function(
  temp_df,
  optFunc = opt_func,
  opt_limits = c(100, 500),
  opt_tolerance = 0.001,
  opt_debug = FALSE
)
{
  opt <- stats::optimise(
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
