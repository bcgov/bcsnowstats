#' Function for formatting data for SWE normal calculation
#' Function to put right columns in data and take daily average if hourly data exists for ASWE data. Data formatting is done prior to any data filling or calculation of normals
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param data data that you are formatting
#' @export
#' @keywords internal
#' @examples \dontrun{}

data_massage <- function(data) {

  data_original_function <- data # save original data in case!

  # Check to make sure that data has a wr column
  if ("wr" %in% colnames(data)) {
  } else {
    data$wr <- bcsnowdata::wtr_yr(dates = data$date_utc)
  }

  # Check to make sure there is a m-d column in the data
  if ("m_d" %in% colnames(data)) {
  } else {
    data <- data %>%
      dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))
  }

  # Detect whether the data is hourly or daily. If it is hourly, take the daily average
  data_mean <- data %>%
    dplyr::group_by(station_id) %>%
    dplyr::mutate(date_only = as.Date(date_utc)) %>%
    dplyr::mutate(timestep = c(0, diff(date_only))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(station_id, date_only) %>%
    dplyr::summarize(swe_mean = mean(value, na.rm = TRUE), .groups = 'keep') %>%
    dplyr::ungroup() %>%
    dplyr::mutate(m_d = format.Date(date_only, "%m-%d")) %>% # add in m_d vector
    dplyr::mutate(wr = bcsnowdata::wtr_yr(dates = date_only)) %>%
    dplyr::rename(date_utc = date_only) %>%
    dplyr::mutate(station_id = unique(data_original_function$station_id, na.rm = TRUE)[1]) %>%
    dplyr::mutate(variable = unique(data_original_function$variable, na.rm = TRUE)[1]) %>%
    dplyr::mutate(station_name = unique(data_original_function$station_name, na.rm = TRUE)[1])

}
