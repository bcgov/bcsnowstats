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
# ===================================

# ===================================
#' Internal function for calculating daily stats for manual sites. Inputs include the data you want to calculate the daily stats for. Including quartiles and stats for all data.
#' Depends on the SWE_normals() function to calculate normals
#' January 2021, Ashlee Jollymore
#' @param data Data that you are calculating statistics for.
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @export
#' @keywords internal
#' @examples \dontrun{}

# ===================================
# Calculating statistics for manual stations
# ===================================
snow_stats_manual <- function(data, normal_min, normal_max) {

  # compute historical stats - for each day of the year
  # get historic dataset - previous to this year
  df_hist <- data %>%
    dplyr::filter(wr < bcsnowdata::wtr_yr(Sys.Date())) %>% # get only 16:00 daily measurement.
    dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))  %>%
    dplyr::filter(!is.na(swe_mm)) %>% # filter out missing data
    dplyr::mutate(swe_mm = as.numeric(as.character(swe_mm))) %>%
    dplyr::group_by(station_id, survey_period)

  # compute historical stats - by survey period
  df_stat <- do.call(data.frame,
                     list(dplyr::summarise(df_hist, min = min(swe_mm, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, swe_mean = mean(swe_mm, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, Q5 = quantile(swe_mm, 0.05, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, Q10 = quantile(swe_mm, 0.1, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, Q25 = quantile(swe_mm, 0.25, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, Q50 = quantile(swe_mm, 0.5,na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, Q75 = quantile(swe_mm,0.75, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, Q90 = quantile(swe_mm,0.90, na.rm = TRUE), .groups = "keep"),
                          dplyr::summarise(df_hist, max = max(swe_mm, na.rm = TRUE), .groups = "keep"))) %>%
    dplyr::select(-survey_period.1, -survey_period.2, -survey_period.3, -survey_period.4, -survey_period.5, -survey_period.6, -survey_period.7, -survey_period.8) %>%
    dplyr::select(-station_id.1, -station_id.2, -station_id.3, -station_id.4, -station_id.5, -station_id.6, -station_id.7, -station_id.8)

  df_time <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(station_id) %>%
    dplyr::summarize(maxdate = max(date_utc), mindate = min(date_utc), .groups = "keep") %>%
    dplyr::mutate(data_range = (paste0(mindate, " to ", maxdate))) %>%
    dplyr::mutate(numberofyears = lubridate::year(maxdate) - lubridate::year(mindate)) %>%
    dplyr::select(station_id, data_range, numberofyears)

  df_stat_date <- dplyr::full_join(df_stat, df_time, by = c("station_id"))

  # get the day of the max and min!!
  min_date <- df_hist %>%
    dplyr::group_by(station_id, survey_period) %>%
    dplyr::slice(which.min(swe_mm)) %>%
    dplyr::select(date_utc, station_id, survey_period) %>%
    dplyr::rename(date_min_utc = date_utc)

  max_date <- df_hist %>%
    dplyr::group_by(station_id, survey_period) %>%
    dplyr::slice(which.max(swe_mm)) %>%
    dplyr::select(date_utc, station_id, survey_period) %>%
    dplyr::rename(date_max_utc = date_utc)

  # append to data
  dates <- dplyr::full_join(min_date, max_date, by = c("survey_period", "station_id"))
  df_stat_1 <- dplyr::full_join(df_stat_date, dates, by = c("survey_period", "station_id"))

  # Calculate the snow normals - 1981 to 2010 (water year)
  df_normals_1 <- SWE_normals(data, normal_max, normal_min)

  # Merge with stats table
  df_stats_all <- dplyr::full_join(df_stat_1, df_normals_1)

  # append the daily mean SWE to a column so that the percentile can be easily calculated within the SBI function
  df_hist_SWE <- df_hist %>%
    dplyr::group_by(station_id, survey_period) %>%
    dplyr::select(survey_period, swe_mm, station_id) %>%
    dplyr::rename(mean_swe = swe_mm) %>%
    tidyr::nest() %>%
    dplyr::select(station_id, survey_period, data)

  # append to the stats
  df_stats_final <- dplyr::full_join(df_stats_all, df_hist_SWE) %>%
    dplyr::rename(historic_swe = data)

  return(df_stats_final)
}
