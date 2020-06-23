plot_residuals_interactive <- function(prediction_df, binwidth = NULL) {
  
  g <-data.frame(residuals = prediction_df$simulated-prediction_df$observed) %>% 
    ggplot2::ggplot(ggplot2::aes_string(x = "residuals")) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::theme_bw()
  
  plotly::ggplotly(g)
  
}
