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
# ======================

#' Normal Ratio Method
#' Function for calculating the difference in mean max SWE for normal ratio calculation for filling in data for ASWE snow stations with 10-20 years of data
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param station station you are calculating the normal ration fpr
#' @param data_station_oi data from the station of interest; i.e., that you are filling in data for
#' @param normal_max Max year that the normal period spans
#' @param normal_min Max year that the normal period spans
#' @param data_id column name of the data that you are filling
#' @export
#' @keywords internal
#' @examples \dontrun{}

ratio_function <- function(station, data_station_oi, data_id, normal_max, normal_min) {

  # Get the data for an adjacent station using the ad_data function. Will return a dataframe containing daily mean SWE, as well as a timeseries of daily mean SWE values for each julian day filled via linear interpolation and smoothed by 7-day centered rolling mean
  ad_data_all <- ad_data(station, normal_max, normal_min, data_id)
  ad_data_m <- do.call(rbind, ad_data_all[2])

  # If there is no data, assign the data
  if (all(is.na(ad_data_m))) {

    # Empty dataframe
    est_swe <- NA

  } else {

     #OLD METHOD - ----------------------------------- before you took the mean day
     # Calculate the max yearly SWE for the site you are looking at
     # meanmax_yearall <-  max(ad_data_m$mean_swe_day_7_fill_soi, na.rm = TRUE)

    # Have a test to reject data that looks like an outlier? There are some instances where the whole year is 0 that looks suspect
    # Filter years with the outlier data and calculate the mean max yearly SWE

    #if (length(boxplot(meanmax_yearall_ad$max_ad, plot=FALSE)$out) > 0) {
    # meanmax_year_ad <- meanmax_yearall_ad %>%
    #   dplyr::filter(!(max_ad %in% boxplot(meanmax_yearall_ad$max_ad, plot=FALSE)$out)) %>% # filter the outlier years
    #   dplyr::ungroup() %>%
    #   dplyr::summarize(mean_max = mean(max_ad, na.rm = TRUE))
    #} else {
    #  meanmax_year_ad <- meanmax_yearall_ad %>%
    #    dplyr::ungroup() %>%
    #    dplyr::summarize(mean_max = mean(max_ad, na.rm = TRUE))
    #}

    # Calculate the mean max yearly SWE for the site you are looking at - station of interest
    #meanmax_yearall <- data_station_oi %>%
    #  dplyr::ungroup() %>%
    #  dplyr::group_by(wr) %>%
    #  dplyr::filter(!is.na(values_stats)) %>%
    #  dplyr::summarise(max_ad = max(values_stats, na.rm = TRUE), .groups = "keep")

    # filter for outlier years within your station data prior to calculating
   # if (length(boxplot(meanmax_yearall$max_ad, plot=FALSE)$out) > 0) {
    #  meanmax_year <- meanmax_yearall %>%
    #    dplyr::filter(!(max_ad %in% (boxplot(meanmax_yearall$max_ad, plot=FALSE)$out))) %>% # filter the outlier years
    #    dplyr::ungroup() %>%
    #    dplyr::summarize(mean_max = mean(max_ad, na.rm = TRUE))
    #} else {
    #  meanmax_year <- meanmax_yearall %>%
    #    dplyr::ungroup() %>%
    #    dplyr::summarize(mean_max = mean(max_ad, na.rm = TRUE))
    #}

    # Calculate the ratio between the two - station of interest / station you are using to fill in data
   # ratio <- data.frame(ratio = ifelse(!is.na(meanmax_year_ad$mean_max),
  #                  meanmax_year$mean_max / meanmax_year_ad$mean_max, # store with Id of the station you are using to fill in data
  #                  NA)) %>%
  #    dplyr::mutate(adj_station_id = unique(ad_st$station_id))

  #  colname_es <- paste0("estswe_", station)

    # Calculate the ratio * SWE for this station
  #  est_SWE <- ad_data %>%
  #    dplyr::mutate(ratio = ratio$ratio) %>%
  #    dplyr::mutate(estimated_swe = values_stats_ad*ratio) %>%
  #    dplyr::select(station_id, date_utc, estimated_swe)
    #  -----------------------------------
    # NEW METHOD - calculate the ratio between the two by each dat
    ratio <- dplyr::full_join(data_station_oi, ad_data_m) %>%
      dplyr::mutate(ratio = mean_swe_day_7_fill / mean_swe_7_fill_ad) %>%
      dplyr::select(m_d, ratio)

    # Calculate the estimated SWE using the raw data from the station of interest
    col_name <- paste0(station, "_estimatedSWE")

    est_swe <- dplyr::full_join(do.call(rbind, ad_data_all[1]), ratio) %>%
      dplyr::mutate(estimated_swe = values_stats_ad * ratio) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, estimated_swe) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::mutate(estimated_swe = as.numeric(estimated_swe)) %>%
      dplyr::mutate(station_id = unique(station))
  }

  return(est_swe)
}
