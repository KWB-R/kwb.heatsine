# get_path_to_excel_file -------------------------------------------------------

get_path_to_excel_file <- function(data_dir, pattern = "^Temperatur.*\\.xlsx$")
{
  xls_files <- dir(data_dir, pattern, full.names = TRUE)

  stopifnot(length(xls_files) == 1L)

  xls_files[[1L]]
}


# load_temperature_from_excel --------------------------------------------------
load_temperature_from_excel <- function(dir_path = "~/../Downloads/kwb-cloud/projects/smart-control")
{
  data_dir <- kwb.utils::safePath(dir_path)

  # Try to find the Excel file
  xls_file <- get_path_to_excel_file(data_dir)

  # Read the temperature data from the Excel file
  temp <- read_temperature_from_xls(xls_file, split_site = FALSE)

  stopifnot(all(kwb.utils::selectColumns(temp, "variable") == "temperature"))

  ggplot2::ggplot(temp, ggplot2::aes(date, value, col = site_id)) +
    ggplot2::geom_line()

  # Just for compliance with the script in its old form, should be removed
  # once this script is clean!
  temp <- kwb.utils::renameColumns(temp, list(
    site_id = "Name_Messstelle",
    value = "Temperatur",
    date = "PN_Datum"
  ))

  temp
}
