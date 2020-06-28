# plot_residuals_interactive ---------------------------------------------------
plot_residuals_interactive <- function(prediction_df, binwidth = NULL)
{
  get_residuals <- function(df) {
    data.frame(residuals = df$simulated - df$observed)
  }

  g <- get_residuals(df = prediction_df) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "residuals")) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::theme_bw()

  plotly::ggplotly(g)
}
