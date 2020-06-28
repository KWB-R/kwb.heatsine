#
# # plot_confidence_interval -----------------------------------------------------
# plot_confidence_interval <- function(year)
# {
#   mean_year <- mean(year)
#   two_sd_year <- 2 * sd(year)
#
#   segments(
#     x0 = mean_year - two_sd_year,
#     y0 = 1,
#     x1 = mean_year + two_sd_year,
#     y1 = 1,
#     col = "red",
#     lwd = 4
#   )
# }

# calculate_sinus --------------------------------------------------------------
calculate_sinus <- function (y0, a, x0, p, x)
{
  y0 + a * sin(2*pi/p * x + x0)
}

# run_montecarlo ----------------------------------------------------------------
run_montecarlo <- function(sinus_fit_list, nMC)
{
  sinus_prediction <- as.data.frame(predict(
    object = sinus_fit_list$lm_object,
    interval = "confidence"
  ))

  sd_df <- tibble::tibble(
    sd_y0 = 0,
    sd_a = 0,
    sd_x0 = 0,
    sd_p = 0,
    sd_sigma = mean(sinus_prediction$fit-sinus_prediction$lwr)
  )

  # sd_df <- tibble::tibble(
  #   sd_y0 = 0,
  #   sd_a = 0,
  #   sd_x0 = 0,
  #   sd_p = 0,
  #   sd_sigma = sd(sinus_fit_list$lm_object$residuals)
  # )

  paras <- sinus_fit_list$paras %>%
    dplyr::bind_cols(sd_df)

  # mx <- NULL
  # my <- NULL
  # year <- NULL

  rnorm_1 <- function(mean, sd, ...) rnorm(1, mean = mean, sd = sd, ...)

  dplyr::bind_rows(lapply(seq_len(nMC), function(run_id) {

    # generate random parameter values
    number_of_days <- length(sinus_fit_list$lm_object$model$value)
    x <- seq_len(number_of_days)
    y0_r <- rnorm_1(paras$y0, paras$sd_y0)
    a_r  <- rnorm_1(paras$a, paras$sd_a)
    x0_r <- rnorm_1(paras$x0, paras$sd_x0)
    p_r  <- rnorm_1(paras$period_length, paras$sd_p)

    # calculate output
    y_r <- calculate_sinus(y0 = y0_r, a = a_r, x0_r, p_r, x)
    #plot(x, y_r)
    sigma_r <- rnorm(number_of_days, 0, sd = paras$sd_sigma)
    #plot(x, y_r + sigma_r)
    #y_r <- y_r + sigma_r

    # my <- c(my, y_r)

    # get position (year) of extreme value minimum
    # positionmini <- order(y_r, decreasing = FALSE)[1L]

    # get position (year) of extreme value maximum
    # positionmini <- order(y_r, decreasing = TRUE)[1L]

    # maxpos <- mean(positionmini)

    #maxpos <- which.min(y_r)

    # year <- c(year, maxpos)
    #
    # # create vector x for plotting
    # mx <- c(mx, x)

    tibble::tibble(
      run_id = run_id,
      day_number =  x,
      y_r = y_r,
      sigma_r = sigma_r,
      y_r_plus_sigma_r = y_r + sigma_r
    )
  }))
}
