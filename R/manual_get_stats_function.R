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

#==========================================================================================================================
# Function for calculating the quartiles and percentiles for each day for each site from manual sites
#==========================================================================================================================

#===============
#' Function for calling withing lapply to calculate statistics for each station - manual stations. Internal function
#' Part of calculating normals workflow for manual data.
#' January 2021, Ashlee Jollymore
#' @param data data you are calculating statistics for
#' @param stations list of station(s) you want to calculate statistics for
#' @param survey_period Survey period you want to calculate statistics for
#' @param get_year water year you want to calculate statistics for
#' @param normal_min minimum year of normal time span
#' @param normal_max max year of normal time span
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_get_stats <- function(data, stations, survey_period, get_year, normal_min, normal_max, ...){

  # Filter the entire manual dataset by the stations you are looking for
  df_tmp <- data %>%
    dplyr::filter(id %in% stations)

  df_tmp$wr <- bcsnowdata::wtr_yr(dates = df_tmp$date_utc)

  df_tmp_1 <- df_tmp %>%
    dplyr::group_by(survey_period, id) %>%
    dplyr::filter(!is.na(swe_mm)) # filter out missing data

  # Calculate statistics and normals through function for each survey period
  df_stat <- snow_stats_manual(data = df_tmp_1, normal_min, normal_max)

  if (dim(df_stat)[1] > 1) {
    # join statistics table with the entire record and calculate percentile for each day
    df_tmp_2 <- dplyr::full_join(df_tmp_1, df_stat, by = c("station_id", "survey_period"))

    #Select the user defined time interval
    if (survey_period == "latest") {
      #Select the last day there is data for
      latest_stats <- df_tmp_2[which(df_tmp_2$date_utc == max(df_tmp_2$date_utc)), ] #select the most recent day of data
      # Calculate the percentile for today

    } else if (survey_period == "All") {
      # return all data with stats
      latest_stats <- df_tmp_2
      # is it worth it to calculate the percentiles for the large dataset?!

    } else {
      sp <- survey_period

      # Select the day and time from the entire data
      latest_stats <- df_tmp_2 %>%
        dplyr::filter(survey_period %in% as.character(tidyselect::all_of(sp)))

      # Calculate the percentile for the survey period you have defined
    }

    # Subset by years selected
    if (get_year == "All") {
      latest_stats_1 <- latest_stats
    } else {
      latest_stats$wr <- bcsnowdata::wtr_yr(latest_stats$date_utc)
      latest_stats_1 <- latest_stats %>%
        dplyr::filter(wr %in% get_year) %>%
        dplyr::select(-wr)
    }

    # Calculate the stats for the day if there is data for today's survey period
    if (dim(latest_stats_1)[1] > 0) {
      latest_stats_day <- latest_stats_1 %>%
        #dplyr::mutate(Data_Range = print(paste0(as.Date(min(Date_UTC)), ' to ', as.Date(max(Date_UTC))))) %>%
        dplyr::mutate(percent_Q50 = round((swe_mm / Q50 * 100), digits = 2)) %>%
        dplyr::mutate(percent_mean = round((swe_mm / swe_mean * 100), digits = 2)) %>%
        dplyr::mutate(percent_normal_mean = ifelse("normal_swe_mean" %in% colnames(latest_stats_1),
                                                   round((swe_mm / normal_swe_mean * 100), digits = 2),
                                                   NA)) %>%
        dplyr::mutate(percent_normal_median = ifelse("normal_Q50" %in% colnames(latest_stats_1),
                                                     round((swe_mm / normal_Q50 * 100), digits = 2),
                                                     NA)) %>%
        dplyr::arrange(date_utc)

      # Calculate the percentile by day - historic SWE in the dataframe
      latest_stats_2 <- latest_stats_day %>%
        dplyr::group_by(station_id, survey_period) %>%
        dplyr::mutate(percentile = ifelse(length(unlist(historic_swe)) > 5,
                                          round(purrr::map2_dbl(historic_swe, swe_mm, ~ecdf(.x$mean_swe)(.y)) * 100, digits = 2),
                                          NaN)) %>%
        dplyr::select(-historic_swe) %>%
        dplyr::arrange(station_id, date_utc)

      # Calculate the rank - min and max for POR
      latest_stats_2$current_rank_min <- mapply(rank_min_function, latest_stats_day$historic_swe, latest_stats_day$swe_mm)

      latest_stats_2$current_rank_max <- mapply(rank_max_function, latest_stats_day$historic_swe, latest_stats_day$swe_mm)


    } else {
      latest_stats_2 <- data.frame() # empty dataframe if no data available
    }

  } else {
    latest_stats_2 <- data.frame() # empty dataframe if no data available
  }

  if (dim(latest_stats_2)[1] < 1) {
    entry <- t(data.frame(c(as.character(stations), survey_period)))
    colnames(entry) <- c("station_id", "survey_period")

    latest_stats_3 <- dplyr::bind_rows(latest_stats_2, as.data.frame(entry))
  } else {
    latest_stats_3 <- latest_stats_2
  }
} # end of function
