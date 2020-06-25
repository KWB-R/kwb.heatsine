#' Load Temperature Data From CSV
#'
#' @param path path to csv file with temperature data and columns: "date" (YYYY-MM-DD) and "value"
#' @return tibble with temperature data and columns "date" and "value"
#' @export
#' @importFrom readr read_csv
#' @importFrom kwb.utils safePath
#' @importFrom stringr str_split_fixed
#' @examples
#' path <- kwb.heatsine::extdata_file("temperature_groundwater_TEG343.csv")
#' gw_data <- kwb.heatsine::load_temperature_from_csv(path)
#' gw_data
#'
load_temperature_from_csv <- function(path)
{
  path <- kwb.utils::safePath(path)

  filename <- basename(path)

  metadata <- kwb.utils::asNoFactorDataFrame(
    stringr::str_split_fixed(kwb.utils::removeExtension(filename), "_", n = 3)
  )

  names(metadata) <- c("general", "type", "monitoring_id")

  stopifnot(metadata$type %in% c("groundwater", "surface-water"))

  structure(
    readr::read_csv(file = path, col_types = "Dd"),
    filename = filename,
    type = metadata$type,
    monitoring_id = metadata$monitoring_id,
    label = ifelse(
      nzchar(metadata$monitoring_id),
      sprintf("%s (%s)", metadata$type, metadata$monitoring_id),
      metadata$type
    )
  )
}
