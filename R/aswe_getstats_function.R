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

#' Internal function for calculating quartiles and percentiles for each day for each site from automated sites; called within lapply function in stats_aswe() function
#' January 2021, Ashlee Jollymore
#' @param data Data for stations you want to get statistics for
#' @param stations station that you are calculating statistics for
#' @param survey_period survey period in %m-%d format
#' @param get_year water year that you are calculatign statistics for
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @param force Whether to overwrite the cached normal data. Defaults to FALSE
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

aswe_get_stats <- function(data_all, stations, survey_period, get_year, normal_min, normal_max, force = FALSE) {

  print(paste0("Calculating statistics for ", stations))

  if ("id" %in% colnames(data_all)) {

    # Get data for the station in order to calculate statistics
    df_tmp_raw <- data_all %>%
      dplyr::filter(id %in% stations)

    # ===========
    # Preprocessing
    # ===========
    # get water year
    df_tmp_raw$wr <- bcsnowdata::wtr_yr(dates = df_tmp_raw$date_utc)

    # ==
    # get the mean SWE by day, rather than choosing just the 14:00 measurement
    # ==
    df_tmp_1 <- df_tmp_raw %>%
      dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))  %>%
      dplyr::mutate(date_dmy = as.Date(date_utc, format = "%Y-%m-%d")) %>%
      dplyr::mutate(mean_day = value)

    # ========================
    # Fill in any missing data that should be zero
    # When there is no snow at a pillow the SWE value can dip negative. Negative values are automatically cut out by Aquarius threshold rules
    # Condition is to fill in 0 if teh last non na value is 10 mm or less for the same water year
    # ========================

    # Fill in NA values with 0 is there is no snow at the pillow
    if (dim(df_tmp_1)[1] > 0) {
      df_stat_fill <- fillNA0(data = df_tmp_1)
    } else {
      df_stat_fill <- df_tmp_1
    }

    # ========================
    # Manually override any incorrect sites with the right value ****
    # ========================

    # ===========
    # Calculate statistics through function. return a table of statistics for each day of the year
    # ===========
    # Compile this as a cache to speed up the function?
    df_stat <- snow_stats(data = df_stat_fill, data_id = "value", normal_min, normal_max, force)

    # ===========
    # if the statistics function returns data, further calculate statistics for the day you specify
    # ===========
    if (dim(df_stat)[1] > 1) {

      df_tmp_2 <- dplyr::full_join(df_stat_fill, df_stat, by = c("id", "m_d"))

      #Select the user defined time interval
      if (survey_period == "latest") {
        #Select the last day there is data for
        latest_stats <- df_tmp_2[which(df_tmp_2$date_utc == max(df_tmp_2$date_utc)), ] #select the most recent day of data
      } else if (survey_period == "All") {
        # return all data with stats
        latest_stats <- df_tmp_2
      } else {
        # Select the day and time from the entire data
        latest_stats <- df_tmp_2 %>%
          dplyr::filter(m_d %in% survey_period)
      }

      # Subset by years selected
      if (get_year == "All") {
        latest_stats_1 <- latest_stats
      } else {
        latest_stats_1 <- latest_stats %>%
         dplyr::filter(wr %in% bcsnowdata::wtr_yr(as.Date(get_year, format = "%Y")))
      }

      # Calculate statistics for the day you want
      latest_stats_day <- latest_stats_1 %>%
        dplyr::mutate(data_range = paste0(as.Date(min(date_utc)), " to ", as.Date(max(date_utc)))) %>%
        dplyr::mutate(percent_Q50 = round((mean_day / Q50 * 100), digits = 2)) %>%
        dplyr::mutate(percent_mean = round((mean_day / swe_mean * 100), digits = 2)) %>%
        dplyr::mutate(percent_normal_mean = ifelse(!is.na(normal_swe_mean), round((mean_day / normal_swe_mean * 100), digits = 2),
                                                 NA)) %>%
        dplyr::mutate(percent_normal_median = ifelse(!is.na(normal_Q50), round((mean_day / normal_Q50 * 100), digits = 2),
                                                   NA)) %>%
        dplyr::group_by(m_d) %>%
        dplyr::filter(!is.na(mean_day)) %>%
        #dplyr::mutate(prctile = round(ecdf(mean_day)(mean_day)*100, digits = 2)) %>%
        #dplyr::mutate(prctile = round(percent_rank(mean_day)*100, digits = 2)) %>% # try calculating the percentile for a month-day across all years by percent_rank
        dplyr::arrange(id, date_utc)

      # Calculate the percentile by day - historic SWE in the dataframe
      if (dim(latest_stats_day)[1] >= 1) { # make sure there is data within the data frame

        #if(!is.null(latest_stats_day$historic_SWE[[1]]) && unique(latest_stats_day$NumberofYears) >5) { # run ecdf if there is sufficient historic data: > 5 years?
        latest_stats_2 <- latest_stats_day %>%
          dplyr::group_by(id, date_utc) %>%
          dplyr::mutate(percentile = ifelse(unique(numberofyears, na.rm = TRUE) > 5,
                                          round(purrr::map2_dbl(historic_swe, value, ~ecdf(.x$mean_SWE)(.y)) * 100, digits = 2), NA)) %>%
          dplyr::select(-historic_swe) %>%
          dplyr::arrange(id, date_utc)

        # Return rank of current value
        #latest_stats_2$current_rank_min <- lapply(rank(as.numeric(c(paste0(unlist(latest_stats_day$historic_SWE)), latest_stats_day$value)))[length(c(paste0(unlist(latest_stats_day$historic_SWE)), latest_stats_day$value))]

        latest_stats_2$current_rank_min <- mapply(rank_min_function, latest_stats_day$historic_swe, latest_stats_day$value)

        # Return rank of current value
        #latest_stats_2$current_rank_max <- rank(-(as.numeric(c(paste0(unlist(latest_stats_day$historic_SWE)), latest_stats_day$value))))[length(c(paste0(unlist(latest_stats_day$historic_SWE)), latest_stats_day$value))]

        latest_stats_2$current_rank_max <- mapply(rank_max_function, latest_stats_day$historic_swe, latest_stats_day$value)

        # Get the number of days until the peak
        latest_stats_2$daystopeak_mean <- as.Date(paste0(latest_stats_2$date_peak_mean, "-", bcsnowdata::wtr_yr(latest_stats_2$date_utc)), format = "%m-%d-%Y") - as.Date(latest_stats_2$date_utc)

        # Get the number of days until the peak
        latest_stats_2$daystopeak_median <- as.Date(paste0(latest_stats_2$date_peak_median, "-", bcsnowdata::wtr_yr(latest_stats_2$date_utc)), format = "%m-%d-%Y") - as.Date(latest_stats_2$date_utc)

      } else {
        latest_stats_2 <- latest_stats_day %>%
          dplyr::group_by(id, m_d) %>%
          dplyr::mutate(percentile = NA) %>%
          dplyr::mutate(current_rank_min = NA, current_rank_max = NA) %>%
          dplyr::mutate(daystopeak_mean = NA, daystopeak_median = NA) %>%
          dplyr::select(-historic_swe) %>%
          dplyr::arrange(id, date_utc)
      }

    } else { # if there is no statistics available for the site in question
      latest_stats_2 <- data.frame()
    }

    # If there is no data within the dataframe
    if (dim(latest_stats_2)[1] < 1) {
      entry <- t(data.frame(c(as.character(stations), survey_period)))
      colnames(entry) <- c("id", "m_d")
      latest_stats_3 <- dplyr::bind_rows(latest_stats_2, as.data.frame(entry))
    } else {
      latest_stats_3 <- latest_stats_2
    }

    # Ensure that all of the columns have the right class, otherwise they will not unlist as a dataframe
    stats_out <- dplyr::as_tibble(latest_stats_3)
  } else {
    stats_out <- NULL
  }
    stats_out
}
