#' Load Temperature Data From CSV
#'
#' @param path path to csv file with temperature data and columns: "type" (i.e. "groundwater", "surface-water"), "date" (YYYY-MM-DD) and "value"
#' @return tibble with temperature data and columns "type" (i.e. "groundwater", "surface-water"), "date" and "value"
#' @export
#' @importFrom readr read_csv
#' @importFrom kwb.utils safePath
#' @examples
#' path <- kwb.heatsine::extdata_file("temperature_groundwater_TEG343.csv")
#' gw_data <- kwb.heatsine::load_temperature_from_csv(path)
#' gw_data
#'
load_temperature_from_csv <- function(path)
  {
  path <- kwb.utils::safePath(path)

  readr::read_csv(file = path,
                col_types = "cDd")
}
