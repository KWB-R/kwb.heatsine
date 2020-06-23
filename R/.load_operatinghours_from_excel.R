# load_operatinghours_from_excel --------------------------------------------------

load_operatinghours_from_excel <- function(dir_path = "~/../Downloads/kwb-cloud/projects/smart-control")
{
  data_dir <- kwb.utils::safePath(dir_path)

  # Try to find the Excel file
  xls_file <- get_path_to_excel_file(data_dir, "Laufzeit")

  data <- readxl::read_excel(xls_file)
  names(data)[1:3] <- c("Jahr", "Monat", "PN_Datum")
  data$PN_Datum <- as.Date(data$PN_Datum)



  `%>%` <- magrittr::`%>%`

  data %>%
    dplyr::select("PN_Datum",
                  dplyr::starts_with("TEG")) %>%
    tidyr::gather(key = "Name_Messstelle",
                  value = "Betriebsstunden",
                  - PN_Datum)


}
