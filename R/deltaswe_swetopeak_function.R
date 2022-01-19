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

##################################
#' Calculate the difference between the current day and the day of the max SWE. Question of how much more snow could be added given historic record?
#' December 2021, Ashlee Jollymore, RFC
#' @param data_plot_1 data you are calculating the potential change in SWE to peak
#' @param lastday_data last day with SWE data
#' @param date_peak date of peak SWE
#' @export
#' @keywords internal
#' @examples \dontrun{}

deltaSWE_datetopeak <- function(data_plot_1, lastday_data, date_peak){

  data_initial <- data_plot_1 %>%
    dplyr::mutate(m_d = format(date_utc, format = "%m-%d")) %>%
    dplyr::mutate(Date_dmy = as.Date(date_utc)) %>%
    dplyr::mutate(Date_dmy = as.Date(Date_dmy)) %>%
    dplyr::filter(!is.na(wr))

  # Linear interpolation to fill in missing days of data.
  # Create a time series of the snow accomulation months: oct - july
  time_start <- min(data_initial$date_utc)
  time_end <- max(data_initial$date_utc)

  # Get the last data value
  lastnonNa_data <- data_initial$value[max(which(!is.na(data_initial$value)))]

  Date = as.data.frame(seq(as.Date(time_start), as.Date(time_end), by = "day"))
  colnames(Date) <- c('Date_dmy')

  Date <- Date %>%
    dplyr::mutate(Date_dmy = as.Date(Date_dmy))

  # Bind to the daily SWE and perform linear interpolation of missing data
  daily_swe_NA <- dplyr::full_join(data_initial, Date) %>%
    dplyr::arrange(date_utc) %>%
    dplyr::mutate(dailySWE_interp = zoo::na.approx(value, na.rm = FALSE))

  # Get only the first and last day of data
  md_current <- format(as.Date(lastday_data), "%m-%d")
  md_peak <- format(as.Date(date_peak), "%m-%d")

  # Filter the data so that there is only the
  data_diff <- daily_swe_NA %>%
    dplyr::filter(m_d %in% c(md_current, md_peak)) %>%
    dplyr::group_by(wr) %>%
    dplyr::mutate(first_diff = dailySWE_interp - stats::lag(dailySWE_interp)) %>%
    dplyr::filter(!is.na(first_diff)) %>%
    dplyr::ungroup()

  # Do statistics on the difference in SWE accumulation between the last value and the max
  # perform stats
  mean <- dplyr::summarise(data_diff, mean = mean(first_diff, na.rm = TRUE))
  median <- dplyr::summarise(data_diff, median = median(first_diff, na.rm = TRUE))
  min <- dplyr::summarise(data_diff, MIN = min(first_diff, na.rm=TRUE))
  Q5 <- dplyr::summarise(data_diff, Q5 = quantile(first_diff, 0.05, na.rm=TRUE))
  Q10 <- dplyr::summarise(data_diff, Q10 = quantile(first_diff, 0.1, na.rm=TRUE))
  Q25 <- dplyr::summarise(data_diff, Q25 = quantile(first_diff, 0.25, na.rm=TRUE))
  Q50 <- dplyr::summarise(data_diff, Q50 = quantile(first_diff, 0.5,na.rm=TRUE))
  Q75 <- dplyr::summarise(data_diff, Q75 = quantile(first_diff,0.75, na.rm=TRUE))
  Q90 <- dplyr::summarise(data_diff, Q90 = quantile(first_diff,0.90, na.rm=TRUE))
  max <- dplyr::summarise(data_diff, MAX = max(first_diff, na.rm=TRUE))
  n_years <- length(data_diff$first_diff)

  delta_stats <- cbind(date_peak, mean, median, min, Q5, Q25, Q50, Q75, Q90, max, n_years)

  start <- data.frame(as.Date(lastday_data), lastnonNa_data, lastnonNa_data, lastnonNa_data, lastnonNa_data, lastnonNa_data, lastnonNa_data, lastnonNa_data, lastnonNa_data, lastnonNa_data, n_years)

  colnames(start) <- colnames(delta_stats)

  data_out <- rbind(start, delta_stats)
  data_out[3, c(2:10)] <- colSums(data_out[,c(2:10)])
  data_out[3,1] <- data_out[2,1]
  data_out[3,11] <- data_out[2,11]
  data_out<- data_out[-2,]

  return(data_out)
}


# Function for calculating the basin averaged projected accumulation
# TO DO
