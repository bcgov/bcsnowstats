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
# ===================================

# ===================================
#' Internal function for calculating daily stats for ASWE sites. Inputs include the data you want to calculate the daily stats for. Including quartiles and stats for all data.
#' Depends on the SWE_normals() function to calculate normals
#' January 2021, Ashlee Jollymore
#' @param data Data that you are calculating statistics for.
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @param data_id name of the column containing the data you are calculating statistics on
#' @param force Whether to overwrite the cached normal data. Defaults to FALSE
#' @export
#' @keywords internal
#' @examples \dontrun{}

snow_stats <- function(data, normal_min, normal_max, data_id, force = FALSE) {

  # Ensure that current water year is defined
  current_wy <- bcsnowdata::wtr_yr(Sys.Date())

  # compute historical stats - for each day of the year from historical stats (prior to this water year)
  # get historic dataset - previous to this year
  df_hist <- data %>%
    dplyr::filter(wr < current_wy) %>% # Filter out the current year data prior to calculating statistics
    dplyr::group_by(id, m_d) %>% # Group by the station ID and the month/day
    dplyr::rename(values_stats = all_of(data_id)) %>% # The user can define what data to run the statistics on. Usually this is the daily mean
    dplyr::filter(!is.na(values_stats)) # filter out missing data

  # Calculate the statistics on the data you specify
  df_stat <- do.call(data.frame,
                   list(dplyr::summarise(df_hist, min = min(values_stats, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, swe_mean = mean(values_stats, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, Q5 = quantile(values_stats, 0.05, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, Q10 = quantile(values_stats, 0.1, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, Q25 = quantile(values_stats, 0.25, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, Q50 = quantile(values_stats, 0.5,na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, Q75 = quantile(values_stats,0.75, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, Q90 = quantile(values_stats,0.90, na.rm = TRUE), .groups = "keep"),
                        dplyr::summarise(df_hist, max = max(values_stats, na.rm = TRUE), .groups = "keep"))) %>%
    dplyr::select(-m_d.1, -m_d.2, -m_d.3, -m_d.4, -m_d.5, -m_d.6, -m_d.7, -m_d.8) %>%
    dplyr::select(-id.1, -id.2, -id.3, -id.4, -id.5, -id.6, -id.7, -id.8)

  # -------------------------------
  # Get the max, min dates, as well as the data range and number of years
  # Number of years. Implement threshold
  df_time <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(value)) # filter out NA values to get more accurate start and end dates

  # Get the years that have >80% of data coverage during the snow accumulation period! Oct 1 - June 30
  df_time_2 <- df_time %>%
    dplyr::arrange(id, date_utc) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id, wr) %>%
    dplyr::filter(m_d >= "10-01" | m_d <= "06-30") %>%
    dplyr::mutate(percent_available = length(mean_day) / as.numeric(abs(difftime(as.POSIXct("2018-10-01"), as.POSIXct("2019-06-30"), units = "days")))*100) %>%
    dplyr::select(id, wr, percent_available) %>%
    unique() %>%
    dplyr::filter(wr == bcsnowdata::wtr_yr(Sys.Date()) | percent_available >= 50)

  daterange <- df_time_2 %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(maxdate = max(wr), mindate = min(wr), .groups = "keep") %>%
    dplyr::mutate(data_range = (paste0(mindate, " to ", maxdate)))

  numberyears <- df_time_2 %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id) %>%
    dplyr::filter(wr != bcsnowdata::wtr_yr(Sys.Date())) %>%
    unique() %>%
    dplyr::mutate(numberofyears = length(percent_available)) %>%
    dplyr::select(id, numberofyears) %>%
    unique()

  # Bind stats regarding the number of years available together
  df_time <- dplyr::full_join(daterange, numberyears, by = c("id"))

  # Bind to the statistics
  df_stat_date <- dplyr::full_join(df_stat, df_time, by = c("id"))

  # get the day of the max and min!!
  min_date <- df_hist %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id, m_d) %>%
    dplyr::slice(which.min(values_stats)) %>%
    dplyr::select(date_utc, id, m_d) %>%
    dplyr::rename(date_min_utc = date_utc)

  # Get the date of the max value for the date
  max_date <- df_hist %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id, m_d) %>%
    dplyr::slice(which.max(values_stats)) %>%
    dplyr::select(date_utc, id, m_d) %>%
    dplyr::rename(date_max_utc = date_utc)

  # append to data
  dates <- dplyr::full_join(min_date, max_date, by = c("id", "m_d"))
  df_stat_1 <- dplyr::full_join(df_stat_date, dates, by = c("id", "m_d"))

  ### Calculate normals using function.
  # Function contains all of the thresholds, etc that make it
  df_normals_1 <- SWE_normals(data, normal_max, normal_min, force)

  # Merge with stats table
  df_stats_all <- dplyr::full_join(df_stat_1, df_normals_1)

  # append the daily mean SWE to a column so that the percentile can be easily calculated within the SBI function
  df_hist_SWE <- df_hist %>%
    dplyr::group_by(id, m_d) %>%
    dplyr::select(m_d, values_stats, id) %>%
    dplyr::rename(mean_SWE = values_stats) %>%
    tidyr::nest() %>%
    dplyr::select(m_d, data, id)

  # append the normal to the stats
  df_stats_final <- dplyr::full_join(df_stats_all, df_hist_SWE, by = c("id", "m_d")) %>%
    dplyr::rename(historic_swe = data)
    #dplyr::mutate(`Number of years within norm range with >= 80% data` = numberofyears_80)

  # Calculate the date of peak median and date of peak mean
  dm_peak_median_date <- df_stats_final$m_d[df_stats_final$Q50 == max(df_stats_final$Q50)][1]
  dm_peak_mean_date <- df_stats_final$m_d[df_stats_final$swe_mean == max(df_stats_final$swe_mean)][1]

  # Get the max themselves
  peak_median <- max(df_stats_final$Q50)
  peak_mean <- max(df_stats_final$swe_mean)

  peak <- data.frame("date_peak_median" = dm_peak_median_date, "peak_median_swe" = peak_median,
                     "date_peak_mean" = dm_peak_mean_date, "peak_mean_swe" = peak_mean)

  #Remove -inf values and replace with NA
  peak_cleaned <- do.call(data.frame, lapply(peak, function(x) replace(x, is.infinite(x), NA)))

  # Bind to statistics
  if (dim(df_stats_final)[1] > 0) {
    df_stats_final_peak <- cbind(df_stats_final, peak_cleaned)
  } else if (dim(df_stats_final)[1] == 0) {
    df_stats_final[1, ] <- NA
    df_stats_final_peak <- cbind(df_stats_final, peak_cleaned)
  }

  return(df_stats_final_peak)
}
