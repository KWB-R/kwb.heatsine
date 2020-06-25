#' Plot Temperature Interactive
#'
#' @param df data frame with temperature data and columns "date" (YYYY-MM-DD) and "value"
#' @return plot with interactive temperature data
#' @export
#' @examples
#' path <- kwb.heatsine::extdata_file("temperature_groundwater_TEG343.csv")
#' gw_data <- kwb.heatsine::load_temperature_from_csv(path)
#' kwb.heatsine::plot_temperature_interactive(gw_data)
#'
plot_temperature_interactive <- function(df)
{
  value_range <- range(kwb.utils::selectColumns(df, "value"))

  yintercept <- c(
    value_range[1L], # minimum
    (value_range[1L] + value_range[2L]) / 2, # mean(c(min, max))
    value_range[2L] # maximum
  )

  g <- ggplot2::ggplot(df, ggplot2::aes_string(x = "date", y = "value")) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = yintercept, lty = 2, colour = "grey") +
    ggplot2::scale_x_date(date_labels = "%Y/%m/%d") +
    ggplot2::labs(title = attr(df, "label"), y = "temperature (\u00B0 C)") +
    ggplot2::theme_bw()

  plotly::ggplotly(g)
}
