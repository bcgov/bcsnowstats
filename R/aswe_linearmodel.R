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

# Function for estimating missing SWE data using a linear model

aswe_linearmodel <- function(data_soi, data_soi_m, stations_adj) {

  # Get the data for an adjacent station using the ad_data function. Will return a dataframe containing daily mean SWE, as well as a timeseries of daily mean SWE values for each julian day filled via linear interpolation and smoothed by 7-day centered rolling mean
  data_adj_l <- lapply(stations_adj, ad_data, normal_max, normal_min, data_id)

  # Unfold the formatted data - daily SWE for each julian dat by station (second element in list)
  data_adj <- do.call(rbind, lapply(data_adj_l, `[[`, "formatted"))

  # Find the correlation between the station of interest and adjacent stations with
  data_adj_select <- data_adj %>%
    dplyr::select(station_id_ad, date, m_d, mean_swe_7_fill_ad) %>%
    dplyr::rename(swe_adj = mean_swe_7_fill_ad)

  # If there is data from adjacent sites within the normal range, use it to fill in data.
  if (dim(data_adj_select)[1] > 0) {

    data_adj_cast <- reshape::cast(data_adj_select, date ~ station_id_ad, fun.aggregate = sum, value = c("swe_adj"))

    if ("NA" %in% colnames(data_adj_cast)) {
      data_adj_cast <- data_adj_cast %>%
        dplyr::select(-"NA")
    }

    # Format data from the station of interest (soi)
    data_soi_ms <- data_soi_m %>%
      dplyr::select(station_id, date, mean_swe_day_7_fill) #%>%
     # dplyr::rename(as.character(unique(data_soi_m$station_id)) = mean_swe_day_7_fill)

    # Connect the daily mean SWE for the station of interest to that from the adjacent stations
    data_all <- dplyr::full_join(data_soi_ms, data_adj_cast) #%>%
     # dplyr::ungroup() %>%
     # dplyr::select(-station_id, -date)

    # Check the data against each other
    #ggplot(data = data_all) + geom_point(aes(x = date, y = mean_swe_day_7_fill)) +
    #  geom_point(aes(x = date, y = `1E02P`), colour = "red") +
    #  geom_point(aes(x = date, y = `1E14P`), colour = "blue") +
    #  geom_point(aes(x = date, y = `1F04P`), colour = "yellow") +
    #  geom_point(aes(x = date, y = `2A06P`), colour = "pink") +
    #  geom_point(aes(x = date, y = `2A18P`), colour = "purple") +
    #  geom_point(aes(x = date, y = `2A30P`), colour = "orange")

    # Check for linearity
    #ggplot(data = data_all) +
    #  geom_point(aes(x = mean_swe_day_7_fill, y = `1E02P`), colour = "red") +
    #  geom_point(aes(x = mean_swe_day_7_fill, y = `1E14P`), colour = "blue") +
    #  geom_point(aes(x = mean_swe_day_7_fill, y = `1F04P`), colour = "yellow") +
    #  geom_point(aes(x = mean_swe_day_7_fill, y = `2A06P`), colour = "pink") +
    #  geom_point(aes(x = mean_swe_day_7_fill, y = `2A18P`), colour = "purple") +
    #  geom_point(aes(x = mean_swe_day_7_fill, y = `2A30P`), colour = "orange")

    # ggplot(data = data_all) +
    #  geom_point(aes(x = `1E02P`, y = mean_swe_day_7_fill), colour = "red") +
  #    geom_point(aes(x = `1A01P`, y = `1E02P`), colour = "blue")

    # Use multiple linear regression model to predict the SWE for the station you are looking for.
    # Can only do up to the date of peak SWE! Otherwise data is non-linear.

    peak_max <- which(data_all$mean_swe_day_7_fill %in% max(data_all$mean_swe_day_7_fill, na.rm = TRUE))

    date_max <- data_all[peak_max, ]$date

    data_accum <- data_all %>%
      dplyr::filter(date <= date_max | date >= "2020-10-01") %>%
      dplyr::ungroup() %>%
      dplyr::select(-station_id, -date)

    lm1 <- lm(mean_swe_day_7_fill ~ ., data = data_accum)
    #summary(lm1)$coefficients[1]
    #length(summary(lm1)$coefficients[1])

    cof <- data.frame(summary(lm1)$coefficients) %>%
      dplyr::rename(pr = "Pr...t..") %>%
      dplyr::mutate(pr = as.numeric(pr)) %>%
      dplyr::filter(pr  < 0.05)

    cof$station <- gsub("[[:punct:]]", "", row.names(cof))

    if ("Intercept" %in% cof$station) {
      cof <- cof %>%
        dplyr::filter(station != "Intercept")
    }

    #Remove the stations that are not significantly correlated with the SWE at the station that
    data_accum_sig <- data_accum[, c("mean_swe_day_7_fill", cof$station)]

    lm_sig <- lm(mean_swe_day_7_fill ~ ., data = data_accum_sig)

    # Extract estimated coefficients to make model. Use raw data, rather than mean daily SWE, to do so.
    # Also, ensure that the adjacent station data is 1) smoothed using 7 -day
    raw_adj <- do.call(rbind, lapply(data_adj_l, `[[`, 1)) %>%
      dplyr::group_by(station_id)

    raw_adj_cast <- reshape::cast(raw_adj, date_utc ~ station_id, fun.aggregate = sum, value = c("values_stats_ad"))

    # Join together and remove columns that you don't need prior to applying linear model
    data_accum_raw <- dplyr::full_join(data_soi, raw_adj_cast)  %>%
      dplyr::ungroup() %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(m_d = format.Date(date_utc, "%m-%d")) %>%
      dplyr::filter(m_d <= format.Date(date_max, "%m-%d") | m_d >= "10-01") %>%
      dplyr::select(-station_id, -m_d, -wr, -variable, -station_name, -numberofyears_80_raw)

    # Create a simulated SWE value from correlation relationships - snow accumulation phase
    data_accum_raw$predicted <- predict(lm_sig, newdata = data_accum_raw)
    data_accum_raw$predicted_7 <- zoo::rollmean(data_accum_raw$predicted, k = 7, na.pad = TRUE, align = c("center"))

    # Replace negative with 0's
    data_accum_raw$predicted_7_0 <- ifelse(data_accum_raw$predicted_7 < 0, 0, data_accum_raw$predicted_7)

    # --------------------------------------------
    # Fit for the snow melt season

    data_melt <- data_all %>%
      dplyr::filter(date > date_max & date < "2020-10-01") %>%
      dplyr::ungroup() %>%
      dplyr::select(-station_id, -date)

    #ggplot() +
    #  geom_point(data = data_melt, aes(x = `1A01P`, y = mean_swe_day_7_fill), colour = "black") +
    #  geom_point(data = data_melt, aes(x = `1E02P`, y = log(mean_swe_day_7_fill)), colour = "blue") +
    #  geom_point(data = data_melt, aes(x = `2A21P`, y = log(mean_swe_day_7_fill)), colour = "red") +
     # stat_smooth(method = "lm", formula = y ~ log(x))

    lm_melt <- lm(log(mean_swe_day_7_fill) ~ ., data = data_melt)
    #summary(lm_melt)$coefficients[1]
    #length(summary(lm_melt)$coefficients[1])

    # Get only the stations with significant correlation
    cof_melt <- data.frame(summary(lm_melt)$coefficients) %>%
      dplyr::rename(pr = "Pr...t..") %>%
      dplyr::mutate(pr = as.numeric(pr)) %>%
      dplyr::filter(pr  < 0.05)

    cof_melt$station <- gsub("[[:punct:]]", "", row.names(cof_melt))

    cof_melt <- cof_melt %>%
      dplyr::filter(station != "Intercept")

    #Remove the stations that are not significantly correlated with the SWE at the station that
    data_melt_sig <- data_melt[, c("mean_swe_day_7_fill", paste0(cof_melt$station))]

    lm_melt_sig <- lm(log(mean_swe_day_7_fill) ~ ., data = data_melt_sig)
    #summary(lm_melt_sig)$coefficients[1]

    # Use fit to extract estimated coefficients to make model. Use raw data, rather than mean daily SWE, to do so.
    # Also, ensure that the adjacent station data is 1) smoothed using 7 -day

    # Join together and remove columns that you don't need prior to applying linear model
    data_melt_raw <- dplyr::full_join(data_soi, raw_adj_cast)  %>%
      dplyr::ungroup() %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(m_d = format.Date(date_utc, "%m-%d")) %>%
      dplyr::filter(m_d > format.Date(date_max, "%m-%d") & m_d < "10-01") %>%
      dplyr::select(-station_id, -m_d, -wr, -variable, -station_name, -numberofyears_80_raw)

    # Create a simulated SWE value from correlation relationships - snow accumulation phase
    data_melt_raw$predicted <- predict(lm_melt_sig, newdata = data_melt_raw)
    data_melt_raw$predicted_7 <- zoo::rollmean(data_melt_raw$predicted, k = 7, na.pad = TRUE, align = c("center"))

    # Replace negative with 0's
    data_melt_raw$predicted_7_0 <- ifelse(data_melt_raw$predicted_7 < 0, 0, data_melt_raw$predicted_7)

    # Bind the accumulation and melt season together
    data_accum_melt <- dplyr::full_join(data_accum_raw, data_melt_raw) %>%
      dplyr::arrange(date_utc)

   # ggplot() + geom_point(data = data_accum_melt, aes(x = date_utc, y = predicted_7_0))
    #ggplot() + geom_point(data = data_accum_melt, aes(x = date_utc, y = values_stats))


}
}
