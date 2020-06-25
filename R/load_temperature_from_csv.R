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

  metadata <- as.data.frame(stringr::str_split_fixed(filename, "_", n = 3))

  names(metadata) <- c("general", "type", "monitoring_id")
  metadata[1,] <- sub("\\.csv", "", metadata[1,], ignore.case = TRUE)

  valid_types <- c("groundwater", "surface-water")
  stopifnot(metadata$type %in% c("groundwater", "surface-water"))

  tdata <- readr::read_csv(file = path, col_types = "Dd")

  attr(tdata, "filename") <- filename
  attr(tdata, "type") <- metadata$type
  attr(tdata, "monitoring_id") <- metadata$monitoring_id
  attr(tdata, "label") <- ifelse(metadata$monitoring_id != "",
                                 sprintf("%s (%s)",
                                         metadata$type,
                                         metadata$monitoring_id),
                                 metadata$type)

  return(tdata)

}
