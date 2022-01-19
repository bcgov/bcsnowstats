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
# =========================================

#' Script for calculating normals for both ASWE and manual stations. This function calls on the correct function to fill data and calculate normals depending on whether the station is a manual or aswe site.
#' Part of calculating normals workflow for ASWE data. January 2021, Ashlee Jollymore
#' @param data data from manual or ASWE site(s). Can also be a station ID if the function is being called on its own.
#' @param id station ID of the site you are calculating normals for
#' @param normal_max Max year that the normal period spans
#' @param normal_min Min year that the normal period spans
#' @param force whether you want to update the cache. Defaults to FALSE, or no
#' @keywords calculate statistics for snow normal period
#' @export
#' @examples \dontrun{}

# -----------------------------
# Calculate the snow normals
# -----------------------------

SWE_normals <- function(data, normal_max, normal_min, force = FALSE) {

  # if the user input data as a station name (i.e., the function is being used as a stand alone function), get the data for the station
  if (all(data %in% bcsnowdata::snow_auto_location()$LOCATION_ID)) {
    data_norm <- bcsnowdata::get_aswe_databc(
      station_id = data,
      get_year = "All",
      parameter_id = c("SWE"),
      force = FALSE,
      ask = FALSE
    )
  } else if (all(data %in% bcsnowdata::snow_manual_location()$LOCATION_ID)) {
    data_norm <- bcsnowdata::get_manual_swe(
      station_id = data,
      get_year = "All",
      force = FALSE,
      ask = FALSE)
  } else {
    data_norm <- data
  }

  id <- unique(data_norm$station_id)

  if (dim(data_norm)[1] == 0) {
      df_normals_out <- data.frame(station_id = character())

  } else if (id %in% bcsnowdata::snow_auto_location()$LOCATION_ID) { # Check to see whether the station is a manual or automated station

      data_id <- "value"

      # Use the aswe_normal() function to fill in data (if appropriate) and calculate normals (if there is sufficient data)
      df_normals_out <- aswe_normal(data = data_norm, normal_max, normal_min, data_id, force = force)

  # If the site is manual site
  } else if (id %in% bcsnowdata::snow_manual_location()$LOCATION_ID) {

      data_id <- "swe_mm"

      df_normals_out <- manual_normal_prep(data = data_norm, normal_max = normal_max, normal_min = normal_min, data_id = data_id)
  } else if (id %in% snow_basins()) {

     # if you are trying to simply get the normal for the entire basin, take the average across the data
     df_normals_out <- basin_normal(data = data_norm, normal_max = normal_max, normal_min = normal_min)

  }

  # End of function
  return(df_normals_out)
}