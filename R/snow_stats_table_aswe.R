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

# ================

#' Function for assembling a table of statistics for designated ASWE snow sites. returns a table of daily (ASWE) and periodic (manual) statistics for a selected station
#' Snow statistics workflow, Ashlee Jollymore RFC
#' @param station_id Station ID that you are assembling a statistics table for
#' @param parameter_id Parameter ID that you are getting statistics for. Defaults to "SWE", but can be "Snow_Depth", "Precipitation", "Temperature"
#' @param normal_min Year that the normal range will start
#' @param normal_max Year that the normal range will end
#' @keywords ASWE statistics table
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

# =============
# Functions for  writing tables of data
# =============
snow_stats_table_aswe <- function(station_id, parameter_id = "SWE", normal_min, normal_max) {

  current_wy <- bcsnowdata::wtr_yr(Sys.time())

  # If the input is All, get the station list from the website for current sites
  if (station_id[1] == "All") {
    stations <- bcsnowdata::snow_auto_location()$LOCATION_ID
  } else {
    stations <- unique(station_id)
  }

  df_f <- lapply(stations, stats_table,
                 parameter_id = parameter_id,
                 normal_min, normal_max)

  #df_final <- do.call("rbind", df_f)
  df_final <- purrr::reduce(rbind, df_f) # unfold the list you created within the loop
}


# Function for calling within lapply solution. Internal function.
stats_table <- function(stations, parameter_id, normal_min, normal_max, ...) {

  # Get data - must address whether to use the archive. Much faster to do so
  df_tmp <- bcsnowdata::get_aswe_databc(station_id = stations,
                            get_year = "All",
                            parameter_id = parameter_id,
                            force = FALSE,
                            ask = FALSE)
  # get water year
  df_tmp$wr <- bcsnowdata::wtr_yr(dates = df_tmp$date_utc)

  # Create a m_d column, group by the date, get the mean parameter by day
  df_tmp_1 <- df_tmp %>%
    dplyr::mutate(m_d = format.Date(date_utc, "%m-%d")) %>%
    dplyr::mutate(date_dmy = as.Date(date_utc)) %>%
    dplyr::group_by(date_dmy) %>%
    dplyr::mutate(mean_day = mean(value, na.rm = TRUE)) %>%
    dplyr::distinct(date_dmy, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(m_d)

  # Calculate statistics through function.
  df_stat_all <- snow_stats(data = df_tmp_1,
                            normal_min, normal_max,
                            data_id = "mean_day")

  # Add column for the station ID
  df_stat_all <- df_stat_all %>%
    dplyr::mutate(station_id <- as.character(stations))
}
