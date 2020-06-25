#' Helper function: select timeperiod
#'
#' @param df data frame with date (defined in parameter "col_date)
#' @param date_start start date of selection
#' @param date_end end date of selection. If no value is given 365.25 days after "date_start" will be used
#' (default: as.Date(date_start) + 365.25)
#' @param col_date column for dates (default: "date")
#' @return data frame with selected dates
#' @export
#'
#' @examples
#' path <- kwb.heatsine::extdata_file("temperature_groundwater_TEG343.csv")
#' gw_data <- kwb.heatsine::load_temperature_from_csv(path)
#' gw_data_selected <- kwb.heatsine::select_timeperiod(gw_data,
#' date_start = "2015-12-28", date_end = "2016-12-26")
#' gw_data_selected
#'
select_timeperiod <- function(
  df,
  date_start,
  date_end = as.Date(date_start) + 365.25,
  col_date = "date"
)
{
  date <- dplyr::pull(df, col_date)

  df[date >= date_start & date <= date_end, ]
}
