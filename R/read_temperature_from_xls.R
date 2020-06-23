# read_temperature_from_xls ----------------------------------------------------
read_temperature_from_xls <- function(
  xls_file, long_format = TRUE, split_site = TRUE)
{
  # Read the Excel file
  temperature <- openxlsx::read.xlsx(xls_file)
  
  # Convert the date column from numeric to Date
  dates <- as.Date(temperature[[1L]], origin = "1899-12-30")
  
  # Report about gaps
  report_gaps(dates)
  
  # Set the date column in the data frame
  temperature[[1L]] <- dates
  names(temperature)[1L] <- "date"
  
  if (! long_format) {
    return(temperature)
  }
  
  # Convert wide format to long format
  result <- kwb.utils::hsMatrixToListForm(
    temperature, 
    keyFields = "date", 
    colNamePar = "site_id",
    colNameVal = "value"
  )
  
  # Remove rows without a value
  result <- result[! is.na(result$value), ]
  
  # Add column "variable"
  result <- cbind(result, variable = "temperature", stringsAsFactors = FALSE)
  
  if (split_site) {
    
    # Get the ID of the site
    ids <- kwb.utils::selectColumns(result, "site_id")
    
    unique_ids <- unique(ids)
    
    site_info <- `rownames<-`(split_site_ids(unique_ids), unique_ids)
    
    result <- cbind(result, site_info[ids, ])
  }
  
  site_columns <- grep("^site", names(result), value = TRUE)
  
  kwb.utils::moveColumnsToFront(result, c(site_columns, "variable", "date"))
}

# report_gaps ------------------------------------------------------------------
report_gaps <- function(dates)
{
  secs_per_day <- 86400L
  
  events <- kwb.event::hsEvents(as.POSIXct(dates), secs_per_day, secs_per_day)
  
  if (nrow(events) > 1L) {
    
    message("There are gaps in the time site. The complete periods are:")
    
    print(kwb.event:::hsEventsToUnit(events, "d")[, -(1:2)])
  }
}

# split_site_ids ---------------------------------------------------------------
split_site_ids <- function(ids)
{
  df <- kwb.utils::noFactorDataFrame(
    site_main = substr(ids, 1L, 3L), 
    site_full = substr(ids, 4L, nchar(ids))
  )
  
  parts <- strsplit(df$site_full, "-/")
  has_year <- lengths(parts) > 1L
  
  df$site_base <- sapply(parts, "[", 1L)
  
  df$site_year <- NA_integer_
  df$site_year[has_year] <- as.integer(
    substr(sapply(parts[has_year], "[", 2L), 1L, 4L)
  )
  
  kwb.utils::removeColumns(df, "site_full")
}
