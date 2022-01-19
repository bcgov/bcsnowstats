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

#' Function for retrieving data for a specific station. Site can be either manual or snow site. Used when you don't have data for a specific station - can be used to directly retrieve snow normal data.
#' @param site Site ID. Can be either manual or ASWE site
#' @param normal_max Max year that the normal period spans
#' @param normal_min Min year that the normal period spans
#' @param force whether you want to update the cache. Defaults to FALSE, or no
#' @export
#' @keywords internal
#' @examples \dontrun{}

# Function for calculating normals and saving the data as a csv file. Will save the entire year of normals, as well as pull normals for the 1st and 15th of the month
normal_site <- function(site, normal_max, normal_min, force = FALSE) {

  if (site %in% bcsnowdata::snow_auto_location()$LOCATION_ID) {
   # Get data for site
   data_site <- bcsnowdata::get_aswe_databc(
     station_id = site,
     get_year = "All",
     parameter_id = c("SWE"),
     force = FALSE,
     ask = FALSE
   )
  } else {
    data_site <- bcsnowdata::get_manual_swe(
      station_id = site,
      survey_period = "All",
      get_year = "All",
      force = FALSE,
      ask = FALSE
    )
  }

  # If there is data for a site, calculate a normal:
  if (dim(data_site)[1] > 0) {
    normals_site  <- SWE_normals(data = data_site,
                                 normal_max,
                                 normal_min,
                                 force)
  } else {
    normals_site <- NA
  }

  return(as.data.frame(normals_site))
}
