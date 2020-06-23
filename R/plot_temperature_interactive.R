#' Plot Temperature Interactive
#'
#' @param df data frame with temperature data and columns "type" (i.e. "groundwater", "surface-water"), "date" and "value"
#'
#' @return plot with interactive temperature data
#' @export
#'
plot_temperature_interactive <- function(df) {
  df <- tibble::as_tibble(df)
  g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "date",
                                                      y = "value")) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(min(df$value),
                                       (max(df$value)+min(df$value))/2,
                                       max(df$value)),
                        lty = 2, colour = "grey") +
    ggplot2::scale_x_date(date_labels = "%Y/%m/%d") +
    ggplot2::theme_bw()

  plotly::ggplotly(g)
}
