# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
# ---------------

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
    dplyr::group_by(id, m_d)

  # Calculate the normal statistics for each day of the year
  b_norm <- do.call(data.frame,
                        list(dplyr::summarise(b_data, normal_minimum = min(values_stats, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_swe_mean = mean(values_stats, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q5 = quantile(values_stats, 0.05, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q10 = quantile(values_stats, 0.1, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q25 = quantile(values_stats, 0.25, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q50 = quantile(values_stats, 0.5, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q75 = quantile(values_stats, 0.75, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_Q90 = quantile(values_stats, 0.90, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(b_data, normal_maximum = max(values_stats, na.rm = TRUE), .groups = "keep"))) %>%
    dplyr::select(-id.1, -id.2, -id.3, -id.4, -id.5, -id.6, -id.7, -id.8) %>%
    dplyr::select(-m_d.1, -m_d.2, -m_d.3, -m_d.4, -m_d.5, -m_d.6, -m_d.7, -m_d.8) %>%
    #dplyr::mutate(Data_Range_normal = (paste0(round(normal_minimum, digits = 0), ' to ', round(normal_maximum, digits = 0)))) %>%
    dplyr::mutate(data_range_normal = (paste0(min(lubridate::year(data$date_utc), na.rm = TRUE), " to ", max(lubridate::year(data$date_utc), na.rm = TRUE)))) %>%
    dplyr::mutate("Date Range" = paste0((max(lubridate::year(data$d_m_y), na.rm = TRUE) - min(lubridate::year(data$d_m_y), na.rm = TRUE))))

  # get the day of the max and min!! Only use real data, not estimated
  min_date <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(m_d) %>%
    dplyr::slice(which.min(date_utc)) %>%
    dplyr::select(date_utc, id) %>%
    dplyr::rename(date_min_normal_utc = date_utc)

  max_date <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(m_d) %>%
    dplyr::slice(which.max(date_utc)) %>%
    dplyr::select(date_utc, id) %>%
    dplyr::rename(date_max_normal_utc = date_utc)

  # append to data
  dates <- dplyr::full_join(min_date, max_date, by = c("id", "m_d"))
  df_normals_out <- dplyr::full_join(b_norm, dates, by = c("id", "m_d"))
}
