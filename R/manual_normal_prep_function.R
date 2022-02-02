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

#' Internal function for filling in missing data and calculating normals for manual snow survey data
#' January 2021, Ashlee Jollymore
#' @param data manual snow data you are calculating normals for
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @param data_id column name you are calculating normals for
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_normal_prep <- function(data, normal_max, normal_min, data_id) {

  # Check to make sure that data has a wr column
  if (!("wr" %in% colnames(data))) {
    data$wr <- bcsnowdata::wtr_yr(dates = data$date_utc)
  }

  # Check to make sure there is a m-d column in the data
  if (!("m_d" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))
  }

  if ("swe_mean" %in% colnames(data)) {
    data_id <- "swe_mean" # reassign the data_ID value
  }

  # Filter the data by the normal span that you specify
  df_time <- data %>%
    dplyr::group_by(id, m_d) %>%
    dplyr::rename(values_stats = all_of(data_id))

  # Count the number of measurements per survey period - how many observations are present for each of the survey date?
  num_obs <- df_time %>%
    dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
    dplyr::filter(!is.na(values_stats)) %>%
    dplyr::group_by(survey_period) %>%
    dplyr::mutate(percent_available = length(values_stats) / 30 * 100) %>% # if there is 100% data, should have 30 measurements
    dplyr::mutate(numberofyears_raw = length(values_stats)) %>%
    dplyr::select(survey_period, percent_available, numberofyears_raw) %>%
    unique()

  # Append to the data as the number of raw data years
  df_normal_time <- dplyr::full_join(df_time, num_obs, by = "survey_period") %>%
    dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
    dplyr::select(-percent_available)

  #####################################
  # What survey periods have 10-20 years of data? If they exist, fill them in
  survey_periods_20 <- num_obs %>%
    dplyr::filter(numberofyears_raw < 20 && numberofyears_raw >= 10) %>%
    dplyr::select(survey_period)

  if (dim(survey_periods_20)[1] > 1) {

    # Fill in data from adjacent stations using manual_datafill() function. Only fill for survey periods between 10-20 years of raw data
    all_swe <- manual_datafill(data = dplyr::full_join(df_time, num_obs, by = "survey_period"),
                               normal_max, normal_min, survey_periods_20, num_obs)

    # Group by survey data and filter so you are only calculating normals for stations with at least 20 years of raw+ predicted data
    all_swe_1 <- all_swe %>%
      dplyr::filter(numberofyears_estimated >= 20) %>%
      dplyr::group_by(survey_period)

    # Calculate normals
    df_normals_filled <- manual_normal(data = all_swe_1)

    # Append the survey periods that don't have enough data to calculate a normal
    survey_l20 <- all_swe %>%
      dplyr::ungroup() %>%
      dplyr::filter(numberofyears_estimated < 20) %>%
      dplyr::select(survey_period, numberofyears_estimated, numberofyears_raw, id) %>%
      unique()

    df_normals_10t20 <- dplyr::full_join(df_normals_filled, survey_l20, by = c("survey_period", "numberofyears_estimated", "numberofyears_raw", "id"))

  } else{
    df_normals_10t20 <- data.frame(id = df_normal_time$id)
  }

  #####################################
  # What survey periods have < 10 observations? Do not calculate a normal
  survey_periods_10 <- num_obs %>%
    dplyr::filter(numberofyears_raw < 10) %>%
    dplyr::select(survey_period)

  if (dim(survey_periods_10)[1] > 1) {

    # If all of the survey periods have less than 10 years, do not calculate a normal
    df_normals_10 <- df_normal_time %>%
      dplyr::ungroup() %>%
      dplyr::filter(survey_period %in% survey_periods_10$survey_period) %>%
      dplyr::select(survey_period, numberofyears_raw, id) %>%
      unique() %>%
      dplyr::mutate(numberofyears_estimated = numberofyears_raw,
                    normal_minimum = NA,
                    normal_swe_mean = NA,
                    normal_Q5 = NA,
                    normal_Q10 = NA,
                    normal_Q25 = NA,
                    normal_Q50 = NA,
                    normal_Q75 = NA,
                    normal_Q90 = NA,
                    normal_maximum = NA,
                    data_range_normal = NA,
                    date_min_normal_utc = NA,
                    date_max_normal_utc = NA)

  } else {
    df_normals_10 <- data.frame(id = unique(df_normal_time$id))
  }

  #####################################
  # If there is 20-30 observations for a particular survey period
  survey_periods_30 <- num_obs %>%
    dplyr::filter(numberofyears_raw >= 20) %>%
    dplyr::select(survey_period)

  if (dim(survey_periods_30)[1] >= 1) {

    all_swe_1 <- df_normal_time %>%
      dplyr::filter(survey_period %in% survey_periods_30$survey_period) %>%
      dplyr::mutate(swe_fornormal = values_stats) %>%
      dplyr::mutate(numberofyears_estimated = numberofyears_raw) %>%
      dplyr::group_by(survey_period)

    # Calculate the normal statistics for each day of the year with manual_normal function
    df_normals_20t30 <- manual_normal(data = all_swe_1)
  } else {
    df_normals_20t30 <- data.frame(id = unique(df_normal_time$id))
  }

  # Join all together
  df_10t30 <- dplyr::full_join(df_normals_20t30, df_normals_10t20) %>%
    unique()

  df_all <- dplyr::full_join(df_10t30, df_normals_10) %>%
    unique()
}
