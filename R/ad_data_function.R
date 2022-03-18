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

#' Function for retrieving and formatting data from adjacent stations
#' @param station adjacent station that your are attempting to retrieve data for
#' @param normal_max Max year that the normal period spans
#' @param normal_min Max year that the normal period spans
#' @param data_id column name of the data that you are filling
#' @export
#' @keywords internal
#' @examples \dontrun{}

ad_data <- function(station, normal_max, normal_min, data_id) {

  # Retrieve data for the adjacent station
  ad_st <- bcsnowdata::get_aswe_databc(station_id = station,
                                     get_year = "All",
                                     parameter = "swe",
                                     timestep = "daily"
  )

  # If there is no data, assign the data
  if (dim(ad_st)[1] == 0) {

   # Empty dataframe
   ad_data_m <- NA

  } else {

    # Format station data and filter for normal time period and 80% yearly data coverage
    ad_data <- data_massage(data = ad_st) %>% # add columns and water year
      dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
      dplyr::group_by(id) %>%
      dplyr::rename(values_stats_ad = value) %>%
      dplyr::filter(!is.na(values_stats_ad)) %>% # filter out missing data
      dplyr::ungroup() %>%
      dplyr::group_by(m_d)
     #dplyr::group_by(wr) %>%
     # dplyr::mutate(percent_available = length(values_stats_ad)/365*100) %>%
     # dplyr::filter(percent_available >= 80) %>%  # Filter years that have at least 80% of the data within a give year and take mean max swe for normal period
     #  dplyr::select(-percent_available) %>%
     # dplyr::ungroup()

    if (dim(ad_data)[1] == 0) { # if there is no data within normal period, return NA

     # Empty dataframe
     ad_data_m <- NA

    } else {

     # Because of data quality, create a mean daily SWE value that you will use to calculate ratio for
     ad_data_m <- do.call(data.frame,
                          list(dplyr::summarise(ad_data, mean_swe_day_ad = mean(values_stats_ad, na.rm = TRUE), .groups = "keep"))) %>%
       dplyr::mutate(date = as.Date(paste0("2020-", m_d))) %>%
       dplyr::mutate(mean_swe_day_7_ad = zoo::rollmean(mean_swe_day_ad, k = 7, na.pad = TRUE, align = c("center"))) %>% # Apply 7 day smoothing to data
       dplyr::mutate(mean_swe_day_7_ad = ifelse(is.na(mean_swe_day_7_ad), mean_swe_day_ad, mean_swe_day_7_ad)) %>% # Get rid of leading NA values
       dplyr::mutate(station_id_ad = unique(station))

     # Fill any missing data using interpolation
     ad_data_m$mean_swe_7_fill_ad <- zoo::na.approx(ad_data_m$mean_swe_day_7_ad, na.rm = T, maxgap = 7)
   }
  }
 return(list(daily_data = ad_data, formatted =  ad_data_m))
}
