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

# ================

#' Function for assembling a table of statistics for designated manual snow sites. Returns a table of statistics for a selected manual station
#' Snow statistics workflow, Ashlee Jollymore RFC
#' @param station_id Station ID that you are assembling a statistics table for. Can be one or a string of multiple stations
#' @param normal_min Year that the normal range will start
#' @param normal_max Year that the normal range will end
#' @export
#' @keywords internal
#' @examples \dontrun{}

snow_stats_table_manual <- function(station_id, normal_min, normal_max, ...) {

  # If the input is All, get the station list from the website for current sites
  if (station_id[1] == "All") {
    stations <- unique(bcsnowdata::snow_manual_location()$LOCATION_ID)
  } else {
    stations <- unique(station_id)
  }

  df_f <- lapply(stations, stats_table_manual, normal_min, normal_max)

  df_final <- do.call(dplyr::bind_rows, df_f) # unfold the list you created within the loop

  return(df_final)
}

# Function called within loop above - Internal function
stats_table_manual <- function(stations, normal_min, normal_max, ...) {

  # get data without using the cache function
  df_tmp <- bcsnowdata::get_manual_swe(station_id = stations,
                                       survey_period = "All",
                                       get_year = "All",
                                       force = FALSE,
                                       ask = FALSE)

  df_tmp$wr <- bcsnowdata::wtr_yr(dates = df_tmp$date_utc) # get water year

  # Calculate statistics and normals through function for each survey period
  df_stat <- snow_stats_manual(data = df_tmp, normal_min, normal_max)

  # Arrange by survey period
  df_stat_all <- df_stat %>%
    dplyr::arrange(station_id, survey_period)
}
