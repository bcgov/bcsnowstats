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
#' @param force Whether to force the recalculation of the normals. Defaults to FALSE
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_normal_prep <- function(data, normal_max, normal_min, data_id, force = FALSE) {

  # ------------------------- Format data -------------------------
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

  # ------------------------- Cache -------------------------
  # Check to ensure that the manual archived data has been cached on the user's computer and is up to date
  fname <- paste0("manual_norm_archive.rds")
  dir <- data_dir()
  fpath <- file.path(dir, fname)

  # If the file doesn't exists or the user decides to force the download, calculate the normal data for the station and save it
  if (any(!file.exists(fpath)) | force) {

    # Check that the directory exists
    check_write_to_data_dir(dir, ask)

    # Calculate the normal data for all days of the year. Run function over all of the stations within a dataframe
    df_normals_out  <- do.call(rbind,
                                lapply(unique(df_time$id),
                                  calc_man_norm,
                                  df_time,
                                  normal_max, normal_min))

    # Save archive - all data before current year
    saveRDS(df_normals_out, fpath)

  } else {

    # Get the previously cached data
    df_normals_initial <- readRDS(fpath)

    # Check to ensure that the data contains statistics with the right normal range. Filter for the range you are looking for
    check <- df_normals_initial %>%
      dplyr::filter(initial_normal_range == paste0(normal_min, " to ", normal_max)) %>%
      dplyr::filter(id %in% unique(df_time$id))

    # If the archive doesn't have the normal range you want, get the data for your normal range and save it
    if (dim(check)[1] < 1 | !(all(unique(check$id) %in% unique(df_time$id)))) {

      # get normals for the year range you want
      df_normals_out  <- do.call(rbind,
                                 lapply(unique(df_time$id),
                                        calc_man_norm,
                                        df_time,
                                        normal_max, normal_min))

      if (!is.null(df_normals_out)) {
        # Append to the data for the other normal range calculated and save
        df_normals_save <- dplyr::full_join(df_normals_out, df_normals_initial) %>%
          unique() %>%
          dplyr::arrange(id, survey_period)

        saveRDS(df_normals_save, fpath)
      }

    } else {
      df_normals_out <- check
    }
  }
  df_normals_out
}

#' Internal function for actually calculating the manual normal data
#' April 2022, Ashlee Jollymore
#' @param df_time manual snow data you are calculating normals for
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @export
#' @keywords internal
#' @examples \dontrun{}

calc_man_norm <- function(station, df_time, normal_max, normal_min) {

  df_st <- df_time %>%
    dplyr::filter(id %in% station)

  # Count the number of measurements per survey period - how many observations are present for each of the survey date?
  num_obs <- df_st %>%
    dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
    dplyr::filter(!is.na(values_stats)) %>%
    dplyr::group_by(id, survey_period) %>%
    dplyr::mutate(percent_available = length(values_stats) / 30 * 100) %>% # if there is 100% data, should have 30 measurements
    dplyr::mutate(numberofyears_raw = length(values_stats)) %>%
    dplyr::select(survey_period, percent_available, numberofyears_raw) %>%
    unique()

  # Append to the data as the number of raw data years
  df_normal_time <- dplyr::full_join(df_st, num_obs) %>%
    dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
    dplyr::select(-percent_available)

  #####################################
  # What survey periods have 10-20 years of data? If they exist, fill them in
  survey_periods_20 <- num_obs %>%
    dplyr::filter(numberofyears_raw < 20 && numberofyears_raw >= 10) %>%
    dplyr::select(survey_period, id)

  if (dim(survey_periods_20)[1] > 1) {

    # Fill in data from adjacent stations using manual_datafill() function. Only fill for survey periods between 10-20 years of raw data
    all_swe <- manual_datafill(data = dplyr::full_join(df_st, num_obs),
                             normal_max, normal_min, survey_periods_20, num_obs)

    # Group by survey data and filter so you are only calculating normals for stations with at least 20 years of raw+ predicted data
    all_swe_1 <- all_swe %>%
      dplyr::filter(numberofyears_estimated >= 20) %>%
      dplyr::group_by(id, survey_period)

    # Calculate normals
    df_normals_filled <- manual_normal(data = all_swe_1)

    # Append the survey periods that don't have enough data to calculate a normal
    survey_l20 <- all_swe %>%
      dplyr::ungroup() %>%
      dplyr::filter(numberofyears_estimated < 20) %>%
      dplyr::select(survey_period, numberofyears_estimated, numberofyears_raw, id) %>%
      unique()

    df_normals_10t20 <- dplyr::full_join(df_normals_filled, survey_l20)

  } else {
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
      dplyr::filter(survey_period %in% unique(survey_periods_30$survey_period)) %>%
      dplyr::mutate(swe_fornormal = values_stats) %>%
      dplyr::mutate(numberofyears_estimated = numberofyears_raw) %>%
      dplyr::group_by(id, survey_period)

    # Calculate the normal statistics for each day of the year with manual_normal function
    df_normals_20t30 <- manual_normal(data = all_swe_1)
  } else {
    df_normals_20t30 <- data.frame(id = unique(df_normal_time$id))
  }

  # Join all together
  df_10t30 <- dplyr::full_join(df_normals_20t30, df_normals_10t20) %>%
    unique()

  df_all <- dplyr::full_join(df_10t30, df_normals_10) %>%
    unique() %>%
    dplyr::mutate(initial_normal_range = paste0(normal_min, " to ", normal_max))
}
