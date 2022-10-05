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
#' Functions for calculating and retrieving statistics for ASWE snow sites. This is the function that calls on internal functions to calculate and assemble relevant statistics.
#' Snow statistics workflow, Ashlee Jollymore RFC. Started 23Oct2018; continually revised since then.
#' @param station_id Station ID that you are assembling a statistics table for. Can be one or a string of multiple stations
#' @param parameter_id Parameter that you are calculating statistics for. Defauls to "SWE", but can be "Snow_Depth", "Precipitation", or "Temperature"
#' @param survey_period Month and day that you want to calculate data for. Defaults to "All"
#' @param get_year Water year that you want to calculate statistics for. Defaults to "All"
#' @param normal_min Year that the normal range will start
#' @param normal_max Year that the normal range will end
#' @param force Whether to overwrite the cached normal data. Defaults to FALSE
#' @keywords ASWE statistics
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

# =========================
# Function that you can call within the get_SBI_data() function
# =========================

stats_aswe <- function(station_id, parameter_id = "SWE", survey_period = "All", get_year = "All", normal_min, normal_max, force = FALSE) {

  # Current water year from the system time
  current_wy <- bcsnowdata::wtr_yr(Sys.time())

  # If the input is All, get the station list from the website for current sites
  if (any(station_id[1] %in% c("All", "all", "ALL")) || any(station_id[1] %in% c("aswe", "ASWE", "Aswe"))) {
    station_list <- unique(bcsnowdata::snow_auto_location()$LOCATION_ID)
  } else {
    station_list <- unique(station_id)
  }

  # convert the survey_period into the right format (in case the input format is incorrect)
  if (survey_period == "01-Jan"){
    survey_period <- "01-01"
  } else if (survey_period == "01-Feb"){
    survey_period <-  "02-01"
  } else if (survey_period == "01-Mar"){
    survey_period <-  "03-01"
  } else if (survey_period == "01-Apr"){
    survey_period <-  "04-01"
  } else if (survey_period == "01-May"){
    survey_period <-  "05-01"
  } else if (survey_period == "15-May"){
    survey_period <-  "05-15"
  } else if (survey_period == "01-Jun"){
    survey_period <-  "06-01"
  } else if (survey_period == "15-Jun"){
    survey_period <-  "06-15"
  } else if (survey_period == "latest"){
    survey_period <- "latest"
  } else {
    survey_period <- survey_period
  }

  print("get data")
  # Get data for the station in order to calculate statistics
  df <- bcsnowdata::get_aswe_databc(station_id = station_list,
                                            get_year = "All",
                                            parameter = "swe",
                                            timestep = "daily"
  )

  # Getting data at this point truncates data returned! Get data within lapply loop
  # use get_percentile function to calculate statistics for the dates and stations specified
  print("calculate stats")
  df_final_1 <- aswe_get_stats(stations = station_list,
                       data_all = df,
                       survey_period = survey_period,
                       get_year = get_year,
                       normal_min = normal_min, normal_max = normal_max, force = force)

  # create an empty row for stations that did not return any data for the period specified
  if (!isTRUE(all.equal(station_list, unique(df_final_1$id))) | !is.data.frame(df_final_1)) {
    missing <- tibble::tibble(id = station_list[!(station_list %in% unique(df_final_1$id))])
    df_final_2 <- dplyr::bind_rows(df_final_1, missing) %>%
      dplyr::arrange(id)
  } else {
    df_final_2 <- df_final_1
  }
}
