# Copyright 2021 Province of British Columbia
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
#======================

#' Internal function for calculating normals for manual snow stations
#' Part of calculating normals workflow for manual data.
#' January 2021, Ashlee Jollymore
#' @param data data you are calculating statistics for
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_normal <- function(data) {

 # Calculate the normal statistics for each day of the year
 df_normals <- do.call(data.frame,
                      list(dplyr::summarise(data, normal_minimum = min(swe_fornormal, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_swe_mean = mean(swe_fornormal, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_Q5 = quantile(swe_fornormal, 0.05, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_Q10 = quantile(swe_fornormal, 0.1, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_Q25 = quantile(swe_fornormal, 0.25, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_Q50 = quantile(swe_fornormal, 0.5, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_Q75 = quantile(swe_fornormal,0.75, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_Q90 = quantile(swe_fornormal,0.90, na.rm = TRUE), .groups = "keep"),
                           dplyr::summarise(data, normal_maximum = max(swe_fornormal, na.rm = TRUE), .groups = "keep"))) %>%
  dplyr::select(-survey_period.1, -survey_period.2, -survey_period.3, -survey_period.4, -survey_period.5, -survey_period.6, -survey_period.7, -survey_period.8) %>%
  #dplyr::mutate(Data_Range_normal = (paste0(round(normal_minimum, digits = 0), ' to ', round(normal_maximum, digits = 0)))) %>%
  dplyr::mutate(data_range_normal = (paste0(min(lubridate::year(data$date_utc), na.rm = TRUE), " to ", max(lubridate::year(data$date_utc), na.rm = TRUE))))

 # Return the data range for the raw and estimated data used to calculate normals - by survey date
 years <- data %>%
   dplyr::select(survey_period, numberofyears_estimated, numberofyears_raw) %>%
   unique()

 # Join with normals
 df_normals_y <- dplyr::full_join(df_normals, years, by = "survey_period")

 # get the day of the max and min!! Only use real data, not estimated
 min_date <- data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(survey_period) %>%
  dplyr::slice(which.min(values_stats)) %>%
  dplyr::select(date_utc, station_id, survey_period) %>%
  dplyr::rename(date_min_normal_utc = date_utc)

 max_date <- data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(survey_period) %>%
  dplyr::slice(which.max(values_stats)) %>%
  dplyr::select(date_utc, station_id, survey_period) %>%
  dplyr::rename(date_max_normal_utc = date_utc)

 # append to data
 dates <- dplyr::full_join(min_date, max_date, by = c("station_id", "survey_period"))
 df_normals_out <- dplyr::full_join(df_normals_y, dates, by = c("survey_period"))
}
