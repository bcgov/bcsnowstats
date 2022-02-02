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
# =========================================

# Script for calculating the manual snow statistics
# Separated from the main script in March 2020 by Ashlee Jollymore.
# Trying to impose order on the chaos, one commented line at a time
# =========================================

#===============
#' Function for calculating statistics for a certain time period - i.e., April 1 - for manual stations
#' Part of calculating normals workflow for manual data.
#' January 2021, Ashlee Jollymore
#' @param station_id ID of the manual station(s) that you want to calculate statistics for
#' @param survey_period Month-Day that you want to calculate statistcis for.
#' @param get_year Water year that you want to retrieve data for. Defaults to "All"
#' @param normal_min Water year that the normal date range begins
#' @param normal_max Water year that the normal date range ends
#' @param incorrect_sites Sites that appear to have incorrect data for the survey period and year selected. Defaults to NA
#' @param incorrect_data If there appears to be incorrect sites, then the user can input what the SWE data should be (if catalog is wrong)
#' @export
#' @keywords internal
#' @examples \dontrun{}

stats_MSWE <- function(station_id, survey_period,
                       get_year = "All",
                       normal_min, normal_max,
                       incorrect_sites = NA, incorrect_data = NA) {

  # Retrieve data for the stations. Don't use caching
  manual_snow <- bcsnowdata::get_manual_swe(station_id = station_id,
                                            survey_period = "All",
                                            get_year = "All")

  # If the input is All, get the list of stations from the data you just retrieved
  if (station_id[1] == "All") {
    stations <- unique(manual_snow$id)
  } else {
    stations <- unique(station_id)
  }

  # convert the survey_period into the right format (in case the input format is incorrect)
  if (survey_period == "01-01") {
    survey_period <- "01-Jan"
  } else if (survey_period == "02-01") {
    survey_period <-  "01-Feb"
  } else if (survey_period == "03-01") {
    survey_period <-  "01-Mar"
  } else if (survey_period == "04-01") {
    survey_period <-  "01-Apr"
  } else if (survey_period == "05-01") {
    survey_period <-  "01-May"
  } else if (survey_period == "05-15") {
    survey_period <-  "15-May"
  } else if (survey_period == "06-01") {
    survey_period <-  "01-Jun"
  } else if (survey_period == "06-15") {
    survey_period <-  "15-Jun"
  } else if (survey_period == "latest") {
    survey_period <- "latest"
  } else {
    survey_period <- survey_period
  }

  # Replace any missing/incorrect data; manually correct any sites that have incorrect SWE data
  if (any(!is.na(incorrect_sites))) {
    for (f in 1:length(incorrect_sites)) {
      manual_snow$swe_mm[manual_snow$id %in% incorrect_sites[f] & manual_snow$survey_period %in% survey_period & lubridate::year(as.Date(manual_snow$date_utc)) %in% get_year] <- incorrect_data[f]
    }
  }

  # use get_percentile function to calculate statistics for the dates and stations specified
  list_stats <- lapply(stations, manual_get_stats,
                      data = manual_snow,
                      survey_period = survey_period,
                      get_year = get_year,
                      normal_min, normal_max)

  # unfold the list you created
  df_final_1 <- do.call(dplyr::bind_rows, list_stats)

  # create an empty row for stations that did not return any data for the period specified
  if (!isTRUE(all.equal(stations, unique(df_final_1$id))) | !is.data.frame(df_final_1)) {
   missing <- tibble::tibble(station_id = stations[!(stations %in% unique(df_final_1$station_id))])
   df_final_2 <- dplyr::bind_rows(df_final_1, missing)
 } else {
   df_final_2 <- df_final_1
  }

  return(df_final_2)
}
