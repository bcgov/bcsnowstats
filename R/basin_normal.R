#' Script for calculating normal for a basin-averaged SWE; Jan 2021 Ashlee Jollymore, RFC
#' @param data data from basin averaged SWE.
#' @param normal_max Max year that the normal period spans
#' @param normal_min Min year that the normal period spans
#' @keywords internal
#' @export
#' @examples \dontrun{}

basin_normal <- function(data, normal_max, normal_min) {

  # Subset the data by the normal max and normal min time
  b_data <- data %>%
    dplyr::filter(lubridate::year(date_utc) >= normal_min & lubridate::year(date_utc) <= normal_max) %>%
    dplyr::group_by(station_id, m_d)

  # Calculate the normal statistics for each day of the year
  b_norm <- do.call(data.frame,
                        list(dplyr::summarise(b_data, normal_minimum = min(mean_day, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_swe_mean = mean(mean_day, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q5 = quantile(mean_day, 0.05, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q10 = quantile(mean_day, 0.1, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q25 = quantile(mean_day, 0.25, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q50 = quantile(mean_day, 0.5, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q75 = quantile(mean_day,0.75, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q90 = quantile(mean_day,0.90, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_maximum = max(mean_day, na.rm = TRUE), .groups = "keep"))) %>%
    dplyr::select(-station_id.1, -station_id.2, -station_id.3, -station_id.4, -station_id.5, -station_id.6, -station_id.7, -station_id.8) %>%
    dplyr::select(-m_d.1, -m_d.2, -m_d.3, -m_d.4, -m_d.5, -m_d.6, -m_d.7, -m_d.8) %>%
    #dplyr::mutate(Data_Range_normal = (paste0(round(normal_minimum, digits = 0), ' to ', round(normal_maximum, digits = 0)))) %>%
    dplyr::mutate(data_range_normal = (paste0(min(lubridate::year(data$date_utc), na.rm = TRUE), " to ", max(lubridate::year(data$date_utc), na.rm = TRUE)))) %>%
    dplyr::mutate("Date Range" = paste0((max(lubridate::year(data$d_m_y), na.rm = TRUE) - min(lubridate::year(data$d_m_y), na.rm = TRUE))))

  # get the day of the max and min!! Only use real data, not estimated
  min_date <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(m_d) %>%
    dplyr::slice(which.min(date_utc)) %>%
    dplyr::select(date_utc, station_id) %>%
    dplyr::rename(date_min_normal_utc = date_utc)

  max_date <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(m_d) %>%
    dplyr::slice(which.max(date_utc)) %>%
    dplyr::select(date_utc, station_id) %>%
    dplyr::rename(date_max_normal_utc = date_utc)

  # append to data
  dates <- dplyr::full_join(min_date, max_date, by = c("station_id", "m_d"))
  df_normals_out <- dplyr::full_join(b_norm, dates, by = c("station_id", "m_d"))
}
