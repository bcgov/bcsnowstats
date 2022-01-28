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
  # Calculate lag
  data$lag <- as.numeric(data$date_utc - lag(data$date_utc), units = "hours")

  if (any((data$lag == 24))) {
    data_mean <- data %>%
      dplyr::select(-lag)
  } else {
    data_mean <- data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(date_only = as.Date(date_utc)) %>%
      dplyr::mutate(timestep = c(0, diff(date_only))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(id, date_only) %>%
      dplyr::summarize(swe_mean = mean(value, na.rm = TRUE), .groups = 'keep') %>%
      dplyr::ungroup() %>%
      dplyr::mutate(m_d = format.Date(date_only, "%m-%d")) %>% # add in m_d vector
      dplyr::mutate(wr = bcsnowdata::wtr_yr(dates = date_only)) %>%
      dplyr::rename(date_utc = date_only) %>%
      dplyr::mutate(id = unique(data_original_function$id, na.rm = TRUE)[1]) %>%
      dplyr::mutate(variable = unique(data_original_function$variable, na.rm = TRUE)[1]) %>%
      dplyr::mutate(station_name = unique(data_original_function$station_name, na.rm = TRUE)[1]) %>%
      dplyr::select(-lag)
  }
}
