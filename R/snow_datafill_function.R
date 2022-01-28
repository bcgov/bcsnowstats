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
# =================

#' Datafilling function
#' Internal function for taking aswe data that has between 10-20 years of a normal period and extending that dataset using data from ASWE sites that are within 100 km.
#' Method uses the normal ratio method to compare site of interest to adjacent sites and calculate estimated dataset.
#' Station of interest (soi) = station you are trying to find data for.
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param data_soi data from the station you are trying to fill in data for
#' @param data_id column name of the data that you wish to fill in data for prior to calculating statistics
#' @param normal_max Max year that the normal period truncates at.
#' @param normal_min Min year that the normal period truncates at.
#' @export
#' @keywords internal
#' @examples \dontrun{}

snow_datafill <- function(data_soi, data_id, normal_max, normal_min, ...) {

  #It is a ASWE site? If so, fill first with manual station if converted, then data from adjacent sites
  if (unique(data_soi$id) %in% bcsnowdata::snow_auto_location()$LOCATION_ID) {

   # Was the site converted from a manual site to a ASWE site? Get the name without the P (P shows that the site is ASWE)
   station_id_manual <- substring(unique(data_soi$id), 1, 4)

   # COMMENTED OUT FOR NOW - not functional. Skip right to filling with nearest neighbours
   #if (station_id_manual %in% bcsnowdata::snow_manual_location()$LOCATION_ID) {
     # backextend data using manual site if converted. Use function
   #}

   # ==================
   # Fill in any missing data with data interpolated from stations within 100km of the station using a normal ratio method
   # Are there stations within 100 km?
   # First, get the location of the station you are looking at
   location_station <- bcsnowdata::snow_auto_location() %>%
     dplyr::filter(LOCATION_ID %in% unique(data_soi$id))
   location_station <- sf::st_as_sf(location_station)

   # All other sites within the vicinity
   location_all <- bcsnowdata::snow_auto_location()
   location_all <- sf::st_as_sf(location_all) %>%
     dplyr::filter(!(LOCATION_ID %in% unique(data_soi$id)))

   # 100 km buffer around the site
   fr_buffer <- sf::st_buffer(location_station, dist = 1e5)

   # Filter for those sites within a 100 km buffer
   ASWE_100km <- sf::st_filter(location_all, fr_buffer)

   # plot to test and check
   #ggplot() +
  #  geom_sf(data = location_station, color = "orange") +
  #  geom_sf(data = ASWE_100km, color = "black") +
  #  geom_sf(data = fr_buffer, fill = NA)

   # If there are ASWE sites within a 100 km radius, the, proceed with the normal ratio method to backfill missing data
   if (length(unique(ASWE_100km$LOCATION_ID)) > 0) {

    ############## Get the weight of each station
    # Get the data for the first station
    stations_adj <- unique(ASWE_100km$LOCATION_ID)

    # use function to return data estimated from normal ratio method using stations within 100 km of the station of interest
    all_swe <- aswe_normalratio(data_soi, stations_adj, data_id, normal_max, normal_min)

  } else {
    # If there are no stations within a 100 km radius and 20-10 years of data, then do not calculate a normal.
    all_swe <- data_soi %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(swe_fornormal = values_stats)  # make a new column that clearly shows the data to use for normal calculation. If there is 20-10 years of data, and no nearby stations to estimate data, no normal calculated
  }

  # ====================
  # Check to see if addition of manual site and the interpolation of adjacent sites has increased the data coverage - all_swe variable
  # Flag the variable if it doesn't have more than 20 years of data, even after data filling
  all_80 <- all_swe %>%
    dplyr::filter(!is.na(swe_fornormal)) %>% # # filter out missing data
    dplyr::ungroup() %>%
    dplyr::group_by(wr) %>%
    dplyr::mutate(percent_available = length(swe_fornormal) / length(seq(as.Date("2020-10-01"), as.Date("2021-06-30"), by = "day")) * 100) %>%
    dplyr::select(wr, percent_available) %>%
    unique() %>%
    dplyr::filter(percent_available >= 80) # filter by the 80% WMO threshold

  # Get the number of years within the normal range with >= 80% data coverage within a specific year
  # Assign a column with the number of years that have 80% of the daily SWE within a snow accumulation year
  all_swe$numberofyears_estimated_80 <- dim(all_80)[1]

  # If the station is a manual station
 } else if (unique(data$id) %in% bcsnowdata::snow_manual_location()$LOCATION_ID) {
  # Interpolation for data from manual stations
  # Does the station have between 10-20 years of data? It is a manual site? Then, extend the data using a correlation method to a neighbouring station.
  # if the station is a manual station with less that 10 years of data
  # correlation relationship, based on a neighboring station with a long period of record.
 }
  # Return the estimated dataset
  return(all_swe)
}
