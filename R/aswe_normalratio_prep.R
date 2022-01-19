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

#' Function for choosing the right method of filling data for ASWE sites
#' This is done as the raw data has a lot of issues - taking the mean by day a way to ensure that the data used to model below is the best quality possible
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param data_soi raw data from the station of interest (soi)
#' @param stations_adj a list of adjacent stations
#' @param data_id what the data ID is
#' @param normal_max year of the max for normal period
#' @param normal_min min year of normal period
#' @export
#' @keywords internal
#' @examples \dontrun{}

aswe_normalratio <- function(data_soi, stations_adj, data_id, normal_max, normal_min) {

  # Format the data from the station of interest  - interpolate for missing data using linear interpolation
  data_soi <- data_soi %>%
    dplyr::ungroup() %>%
    dplyr::mutate(swe_interp = zoo::na.approx(values_stats, na.rm = F, maxgap = 21)) %>%
    dplyr::group_by(m_d)

  # Take the daily mean
  data_soi_m <- do.call(data.frame,
                       list(dplyr::summarise(data_soi, mean_swe_day = mean(values_stats, na.rm = TRUE), .groups = "keep"))) %>%
   dplyr::mutate(date = as.Date(paste0("2020-", m_d))) %>%
   dplyr::mutate(mean_swe_day_7 = zoo::rollmean(mean_swe_day, k = 7, na.pad = TRUE, align = c("center"))) %>% # Apply 7 day smoothing to data
   dplyr::mutate(mean_swe_day_7 = ifelse(is.na(mean_swe_day_7), mean_swe_day, mean_swe_day_7)) %>% # Get rid of leading NA values
   dplyr::mutate(station_id = unique(data_soi$station_id))

  # Fill any missing data using interpolation
  data_soi_m$mean_swe_day_7_fill <- zoo::na.approx(data_soi_m$mean_swe_day_7, na.rm = T, maxgap = 7) # Fill any gaps with linear interpolation

  # ------------------------------------------------------
  # Calculate the estimated SWE using the ratio normal method (for mean SWE for each day)
  ratio_all <- lapply(stations_adj, ratio_function,
                    data_station_oi = data_soi_m,
                    data_id, normal_max, normal_min)

  ratio_all_unfold <- do.call(rbind, ratio_all)

  # Unfold the data
  if (dim(ratio_all_unfold)[1] > 0 & !(all(is.na(ratio_all_unfold)))) { # Calculated the estimated SWE for the entire dataset available (if there is data available!)

   # Unfold the estimated data from the adjacent stations
   estimated_unmelted <- ratio_all_unfold %>%
     dplyr::filter(!is.na(estimated_swe)) %>%
     tidyr::spread(station_id, estimated_swe)

   if ("NA" %in% colnames(estimated_unmelted)) {
      estimated_unmelted <- estimated_unmelted %>%
         dplyr::select(-"NA")
   }

   # If there are multiple stations, take the row mean. If not, use the one station retrieved
   estimated_unmelted_mean <- estimated_unmelted %>%
      dplyr::mutate(mean_est_swe = rowMeans(dplyr::select(., -date_utc), na.rm = TRUE)) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) #%>%
       #dplyr::mutate(est_swe_5 = zoo::rollmean(mean_est_swe, k = 7, na.pad = TRUE, align = c("center")))


   # Join the two datasets together and create a column that is the observed data with the estimated filled in
   all_swe_ratio <- dplyr::full_join(data_soi, estimated_unmelted_mean %>% dplyr::select(date_utc, mean_est_swe), by = "date_utc") %>%
     dplyr::arrange(date_utc) %>%
     dplyr::mutate(swe_est = ifelse(is.na(swe_interp), mean_est_swe, swe_interp))  #if there is no interpolated swe value, fill

   all_swe_ratio$swe_est_7 <- zoo::rollmean(as.numeric(all_swe_ratio$swe_est), k = 14, na.pad = TRUE, align = c("center")) # not working in pipe?

   all_swe_ratio <- all_swe_ratio %>%
     dplyr::mutate(swe_fornormal = swe_est_7) %>% # make a new column that clearly shows the data to use for normal calculation
     dplyr::mutate(station_id = unique(data_soi$station_id, na.rm = TRUE)) %>%
     dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
     dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))

   # Plot the estimated dataset versus the observed
   #ggplot() +
  #   geom_point(data = all_swe_ratio, aes(x = date_utc, y = swe_fornormal), colour = "blue") +
    # geom_point(data = all_swe_ratio, aes(x = date_utc, y = values_stats))

  } else {
   # If there are no stations within a 100 km radius and 20-10 years of data THAT HAVE DATA, then do not calculate a normal.
     all_swe_ratio <- data_soi %>%
     dplyr::arrange(date_utc) %>%
     dplyr::mutate(swe_fornormal = values_stats)
  }
  # Put the above into a function?

  # Calculate the standard error for the normal ratio method
  rmse_ratio_data <- all_swe_ratio %>%
    dplyr::mutate(residual = values_stats - swe_fornormal) %>%
    dplyr::filter(!is.na(residual)) %>%
    dplyr::mutate(residual_2 = residual^2)

  all_swe_ratio$rmse_ratio <- sqrt(1 / length(rmse_ratio_data$residual) * sum(rmse_ratio_data$residual_2))

  # -----------------------------------------------
  # Calculate the estimated SWE using a multiple linear regression
  #all_swe_linear <- aswe_linearmodel(data_soi_m, stations_adj)

  # Choose which data to return based on the lowest RMSE

  return(all_swe_ratio)
}
