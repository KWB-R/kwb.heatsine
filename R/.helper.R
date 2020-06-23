# get_path_to_excel_file -------------------------------------------------------

get_path_to_excel_file <- function(data_dir, pattern = "^Temperatur.*\\.xlsx$")
{
  xls_files <- dir(data_dir, pattern, full.names = TRUE)

  stopifnot(length(xls_files) == 1L)

  xls_files[[1L]]
}
