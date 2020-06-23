## Select start day

select_timeperiod <- function(df,
                              date_start,
                              date_end = as.Date(date_start) + 365.25,
                              col_date = "date") {
  date <- dplyr::pull(df, col_date)

  condition <- date >= date_start & date <= date_end

  df[condition, ]
}
