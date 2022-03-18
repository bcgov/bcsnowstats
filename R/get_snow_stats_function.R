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

#' Function for calculating statistics for a snow station
#' Diverts into either the workflow for calculating ASWE or manual snow station
#' November 2021, Ashlee Jollymore
#' @param station_id ID of the manual station(s) that you want to calculate statistics for
#' @param survey_period Month-Day that you want to calculate statistics for.
#' @param get_year Water year that you want to retrieve data for. Defaults to "All"
#' @param normal_min Water year that the normal date range begins
#' @param normal_max Water year that the normal date range ends
#' @param incorrect_sites Sites that appear to have incorrect data for the survey period and year selected. Defaults to NA
#' @param incorrect_data If there appears to be incorrect sites, then the user can input what the SWE data should be (if catalog is wrong)
#' @export
#' @keywords Retrieve snow statistics
#' @examples \dontrun{}

get_snow_stats <- function(station_id = c("all", "aswe", "manual"), survey_period, get_year, normal_min, normal_max, force = TRUE) {

  aswe <- bcsnowdata::snow_auto_location()$LOCATION_ID
  man <- bcsnowdata::snow_manual_location()$LOCATION_ID

  if (any(station_id %in% c("aswe", "ASWE", "Aswe"))) {
    id_aswe <- aswe
  } else if (any(station_id %in% c("manual", "MANUAL", "Manual", "man"))) {
    id_manual <- man
  } else if (any(station_id %in% c("ALL", "all", "All"))) {
    id_aswe <- aswe
    id_manual <- man
  } else {

    # Check to see whether the station is a manual or automated station
    id_aswe <- station_id[station_id %in% aswe]

    id_manual <- station_id[station_id %in% man]
  }

  if (length(id_aswe) > 0) {
  #if (any(station_id %in% c("aswe", "ASWE", "Aswe")) || any(station_id %in% c("ALL", "all", "All")) || any(station_id %in% bcsnowdata::snow_auto_location()$LOCATION_ID)) {
    df_aswe <- stats_aswe(station_id = id_aswe,
               survey_period = survey_period,
               get_year = get_year,
               normal_min = normal_min,
               normal_max = normal_max,
               force = force)
  }
  # Manual data
  if (length(id_manual) > 0) {
  #if (station_id %in% c("manual", "MANUAL", "Manual", "man") || station_id %in% c("ALL", "all", "All") || station_id %in%  bcsnowdata::snow_manual_location()$LOCATION_ID) {
    df_manual <- stats_MSWE(station_id = id_manual,
               survey_period = survey_period,
               get_year = get_year,
               normal_min = normal_min,
               normal_max = normal_max)
  }
  # Bind together
  if (exists("df_aswe") && exists("df_manual")) {
    all <- dplyr::full_join(df_aswe, df_manual)
  } else if (exists("df_aswe") && !exists("df_manual")) {
    all <- df_aswe
  } else if (!exists("df_aswe") && exists("df_manual")) {
    all <- df_manual
  }
}
