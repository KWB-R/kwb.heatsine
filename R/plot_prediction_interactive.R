#' Helper function: mean_cl_quantile
#'
#' @param x x values
#' @param q quantiles (default: c(0.1,0.9))
#' @param na.rm (default: TRUE)
#' @return data frame with y, ymin, ymax
#' @keywords internal
#' @importFrom stats quantile
#'
mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE)
{
  quantiles <- stats::quantile(x, probs = q, na.rm = na.rm)

  data.frame(
    y = mean(x, na.rm = na.rm),
    ymin = quantiles[1L],
    ymax = quantiles[2L]
  )
}

#' Get tidy traveltimes
#'
#' @param traveltimes traveltimes object as retrieved by
#'   \code{\link{get_predictions}}
#'
#' @return data frame with tidy traveltimes
#' @export
#' @importFrom rlang .data
#' @importFrom tidyr gather
#'
get_tidy_traveltimes <- function(traveltimes) {
  tidyr::gather(traveltimes,
    key = "label",
    value = "date",
    -.data$point_type,
    -.data$traveltime_thermal_days,
    -.data$retardation_factor,
    -.data$traveltime_hydraulic_days
  )
}

#' Plot Prediction Interactive
#'
#' @param predictions as retrieved by get_predictions()
#'
#' @return interactive prediction plot
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_rev
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
plot_prediction_interactive <- function(predictions)
{

  traveltimes_tidy <- predictions$traveltimes %>%
    get_tidy_traveltimes() %>%
    dplyr::mutate(
      x_label = sprintf(
        "%s (%s)%s",
        .data$point_type,
        .data$date,
        dplyr::if_else(
          grepl("^groundwater", x = .data$label) ,
          sprintf(
            ": traveltime_thermal: %3.1f days, traveltime_hydraulic: %3.1f days",
            .data$traveltime_thermal_days,
            .data$traveltime_hydraulic_days
          ),
          ""
        )
      )
    ) %>%
    dplyr::left_join(predictions$points[,c("point_type", "label", "date", "simulated")],
                     by = c("point_type", "label", "date"))

  copy_unless_observed <- function(.data, column) {
    ifelse(.data[["temperature"]] == "observed", NA_real_, .data[[column]])
  }

  g1 <- predictions$data %>%
    dplyr::select(-.data$type,
                  -.data$monitoring_id,
                  -.data$residuals) %>%
    tidyr::gather(
      key = "temperature",
      value = "value",
      - .data$label,
      - .data$date,
      - .data$simulated_pi_lower,
      - .data$simulated_pi_upper
    ) %>%
    dplyr::mutate(
      simulated_pi_lower = copy_unless_observed(.data, "simulated_pi_lower"),
      simulated_pi_upper = copy_unless_observed(.data, "simulated_pi_upper")
    ) %>%
    ggplot2::ggplot(ggplot2::aes_string("date", "value", col = "temperature")) +
    ggplot2::facet_wrap(~ forcats::fct_rev(.data$label), ncol = 1) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = traveltimes_tidy, mapping = ggplot2::aes(
       x = .data$date,
       y = .data$simulated,
       #fill = "lower: 5%, upper: 95%",
       col = .data$point_type
      ),
      size = 3,
      alpha = 0.5,
      show.legend = TRUE,
      inherit.aes = FALSE
    ) +
    # ggplot2::geom_text(
    #   data = traveltimes_tidy,
    #   mapping = ggplot2::aes(x = .data$date, y = 0, label = .data$x_labe),
    #   size = 4,
    #   angle = 90,
    #   vjust = -0.4,
    #   hjust = 0
    # ) +
    ggplot2::geom_ribbon(
      ggplot2::aes_string(
        ymin = "simulated_pi_lower",
        ymax = "simulated_pi_upper"
      ),
      linetype = 2,
      alpha = 0.1,
      show.legend = TRUE
    ) +
    ggplot2::ylab(label = "temperature (\u00B0 C)") +
    ggplot2::scale_x_date(date_labels = "%Y/%m/%d") +
    #ggplot2::scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
    # ggplot2::scale_fill_manual(
    #   "", labels = c("Prediction interval"), values = c("grey")
    # ) +
    #ggplot2::theme_bw() +
    #ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = 0)) +
    ggplot2::guides(#fill = ggplot2::guide_legend(title="Prediction interval"),
                    col = ggplot2::guide_legend(title="Data points")) +
    ggplot2::theme_bw()


  plotly::ggplotly(g1)
}
