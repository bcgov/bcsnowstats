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
# ===================

#' Internal function for calculating normals for ASWE sites. Includes data filling for stations with 10-20 years of data.
#' January 2021, Ashlee Jollymore
#' @param df data input
#' @param survey_period survey period in %m-%d format
#' @param get_year water year that you are calculating statistics for
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @param ask asks user whether they want to save data. Defaults to FALSE
#' @param force asks user whether they want to manually overwrite the current cache. Defaults to FALSE (no)
#' @export
#' @keywords internal
#' @examples \dontrun{}

aswe_normal <- function(df, normal_max, normal_min, data_id, ask = FALSE, force = FALSE) {

  # Check cached data to see if the normals have already been calculated for the day of interest

  # Check to ensure that the ASWE archived data has been cached on the user's computer and is up to date
  fname <- paste0(unique(df$parameter), "_norm_archive.rds")
  dir <- data_dir()
  fpath <- file.path(dir, fname)

  # If the file doesn't exists or the user decides to force the download, calculate the normal data for the station and save it
  if (any(!file.exists(fpath)) | force) {

    # Check that the directory exists
    check_write_to_data_dir(dir, ask)

    # Calculate the normal data for all days of the year
    df_normals_out  <- int_aswenorm(data = df, normal_max, normal_min, data_id)

    # Save archive - all data before current year
    saveRDS(df_normals_out, fpath)

  } else {

    # Get the previously cached data
    df_normals_initial <- readRDS(fpath)

    # Check to ensure that the data contains statistics with the right normal range. Filter for the range you are looking for
    check <- df_normals_initial %>%
      dplyr::filter(initial_normal_range == paste0(normal_min, " to ", normal_max)) %>%
      dplyr::filter(id %in% unique(df$id))

    # If the archive doesn't have the normal range you want, get the data for your normal range and save it
    if (dim(check)[1] < 1 | !(all(unique(check$id) %in% unique(df$id)))) {

      # get normals for the year range you want
      df_normals_out  <- int_aswenorm(data = df, normal_max, normal_min, data_id)

      if (!is.null(df_normals_out)) {
        # Append to the data for the other normal range calculated and save
        df_normals_save <- dplyr::full_join(df_normals_out, df_normals_initial) %>%
          unique() %>%
          dplyr::arrange(m_d)

        saveRDS(df_normals_save, fpath)
      }

    } else {
      df_normals_out <- check
    }
  }

  return(df_normals_out)
}

# -------------------------------------
#' Function for actually calculating normals from ASWE stations
#' January 2021, Ashlee Jollymore
#' @param data station that you are calculating statistics for
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @param data_id column name of data you are calculating normals for
#' @export
#' @keywords internal
#' @examples \dontrun{}
#
int_aswenorm <- function(data, normal_max, normal_min, data_id) {

  # Put data into right format using the data_massage function
  data_m <- data_massage(data)

  if ("swe_mean" %in% colnames(data_m)) {
    data_id <- "swe_mean" # reassign the data_ID value
  }

  # Filter the data by the normal span that you specify
  df_normal_time <- data_m %>%
    dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
    dplyr::group_by(id, m_d) %>%
    dplyr::rename(values_stats = all_of(data_id))

  # ++++++++++++++++++++++ thresholds
  # Check to see whether there is sufficient data to calculate a normal.
  # The WMO recommends only calculating a normal for stations that have 80% of the data available
  # Firstly, just show the amount of data available for the normal period
  # Number of years with 80% or great of the data available.
  # Only count the data between Oct-June - doesn't matter if the snow data is missing in summer - 273 days in snow accumulation/melt season
  # Only for ASWE
  df_normal_80 <- df_normal_time %>%
    dplyr::filter(!is.na(values_stats)) %>% # # filter out missing data
    dplyr::ungroup() %>%
    dplyr::group_by(id, wr) %>%
    dplyr::filter(lubridate::month(as.Date(m_d, format = "%m-%d")) <= 6 | lubridate::month(as.Date(m_d, format = "%m-%d")) >= 10) %>% # get only the snow accumulation and melt season
    dplyr::mutate(percent_available = length(values_stats) / length(seq(as.Date("2020-10-01"), as.Date("2021-06-30"), by = "day")) * 100) %>%
    dplyr::select(id, wr, percent_available) %>%
    unique() %>%
    dplyr::filter(percent_available >= 80) # filter by the 80% WMO threshold

  # Get the number of years within the normal range with >= 80% data coverage within a specific year
  ny_80 <- df_normal_80 %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(numberofyears_80_raw = n())

  # Add the number of years with 80% of data to the dataframe
  df_nt <- df_normal_time %>%
    dplyr::full_join(ny_80) %>%
    dplyr::mutate(numberofyears_80_raw = ifelse(is.na(numberofyears_80_raw), 0, numberofyears_80_raw))

  df_normals <- lapply(unique(data$id),
    calc_norm,
    df_nt = df_nt,
    df_normal_80 = df_normal_80, normal_max = normal_max, normal_min = normal_min)

  df_normals_final <- do.call(plyr::rbind.fill, df_normals) %>%
    unique()

  return(df_normals_final)
}

#' Function for defining whether to fill in data and calculate normals. To be run station by station
#' July 2022, Ashlee Jollymore
#' @param station station that you are calculating statistics for
#' @param df_nt data
#' @param df_normal_80 data that reaches the 80% completeness threshold
#' @param normal_min date for the min normal year
#' @param normal_max date of the max normal year
#' @export
#' @keywords internal
#' @examples \dontrun{}
#
calc_norm <- function(stn_id, df_nt, df_normal_80, normal_max, normal_min) {

  numberofyears_80 <- df_nt %>%
    ungroup() %>%
    dplyr::filter(id %in% stn_id) %>%
    dplyr::select(numberofyears_80_raw) %>%
    unique()

  df_normal_time <- df_nt %>%
    dplyr::filter(id %in% stn_id)

  dfn_80 <- df_normal_80 %>%
    dplyr::filter(id %in% stn_id)

  if (dim(numberofyears_80)[1] == 0) {
    numberofyears_80_raw <- 0
  } else {
    numberofyears_80_raw <- numberofyears_80$numberofyears_80_raw
  }

  # =============================
  # Fill in data depending on how many years of data there are available
  # Is there less than 10 years of data?
  if (numberofyears_80_raw < 10) {

    data_0t10 <- df_normal_time %>% # Make a new variable to preserve the initial data - years with at least 80% of the data in the snow accumulation period.
        dplyr::mutate(survey_period = format(date_utc, format = "%d-%b"))
    # ++++++++++++++++++++++++++++++++++++++++++++++
    # Use function to calculate the normal if the stn_id was converted from manual to aswe
    df_normals <- manual_2aswe(id = stn_id, normal_max, normal_min)

    # Filter out the years that have less that 80% of the data within the snow accumulation season; Add in correct columms
    if (is.null(dim(df_normals)[1])) {

      df_normals_out <- data.frame("id"  = stn_id,
                                   "m_d" = NA,
                                   "normal_minimum" = NA,
                                   "normal_swe_mean" = NA,
                                   "normal_Q5" = NA,
                                   "normal_Q10" = NA,
                                   "normal_Q25"  = NA,
                                   "normal_Q50" = NA,
                                   "normal_Q75" = NA,
                                   "normal_Q90" = NA,
                                   "normal_maximum" = NA,
                                   "data_range_normal" = NA,
                                   "initial_normal_range" = paste0(normal_min, " to ", normal_max),
                                   "normal_datarange_estimated" = NA,
                                   "normal_datarange_raw" = NA,
                                   "date_min_normal_utc" = NA,
                                   "date_max_normal_utc"  = NA,
                                   "data_flag" = "insufficient data")

    } else {

      df_normals_out <- df_normals

    }
    all_swe <- NULL
  }

  # Does the station have between 10-20 years of data? If so, extend the dataset using 1) manual dataset (if converted), and 2) adjacent stations
  if (numberofyears_80_raw >= 10 && numberofyears_80_raw < 20) {

    data_20t10 <- df_normal_time # Make a new variable to preserve the initial data - years with at least 80% of the data in the snow accumulation period.

    # Fill in missing data with an estimated dataset from either manual dataset (if converted) and/or adjacent stations
    all_swe <- snow_datafill(data_soi = data_20t10, data_id, normal_max, normal_min) %>%
      dplyr::mutate(data_flag = "10-20 years of data; data filled from adjacent sites")
  }

  # Does the site have between 20-30 years of data? Don't add in any additional data and jsut calculcate normals from
  if (numberofyears_80_raw >= 20 && numberofyears_80_raw <= 30) {

    # DON'T Filter out the years that have less that 80% of the data within the snow accumulation season; Add in correct columns
    all_swe <- df_normal_time %>%
     #dplyr::filter(wr %in% df_normal_80$wr) %>%
      dplyr::mutate(numberofyears_estimated_80 = numberofyears_80$numberofyears_80_raw) %>%
      dplyr::mutate(swe_fornormal = values_stats)%>%
      dplyr::mutate(data_flag = ">20 years of data; no data filling")
  }

  # End of data filling according to thresholds

  # ==============================
  # Calculate normals. Only calculate normal if there is sufficient data
  if (length(all_swe$numberofyears_estimated_80) > 0 && unique(all_swe$numberofyears_estimated_80) >= 20 && unique(all_swe$numberofyears_estimated_80) <= 30 && numberofyears_80_raw >= 10) {

    all_swe_1 <- all_swe %>%
      dplyr::group_by(id, m_d) %>%
      dplyr::filter(!is.na(swe_fornormal))

    # Calculate the normal statistics for each day of the year
    df_normals <- do.call(data.frame,
                        list(dplyr::summarise(all_swe_1, normal_minimum = min(swe_fornormal, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_swe_mean = mean(swe_fornormal, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_Q5 = quantile(swe_fornormal, 0.05, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_Q10 = quantile(swe_fornormal, 0.1, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_Q25 = quantile(swe_fornormal, 0.25, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_Q50 = quantile(swe_fornormal, 0.5, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_Q75 = quantile(swe_fornormal, 0.75, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_Q90 = quantile(swe_fornormal, 0.90, na.rm = TRUE), .groups = "keep"),
                             dplyr::summarise(all_swe_1, normal_maximum = max(swe_fornormal, na.rm = TRUE), .groups = "keep"))) %>%
      dplyr::select(-m_d.1, -m_d.2, -m_d.3, -m_d.4, -m_d.5, -m_d.6, -m_d.7, -m_d.8) %>%
      dplyr::select(-id.1, -id.2, -id.3, -id.4, -id.5, -id.6, -id.7, -id.8) %>%
      #dplyr::mutate(Data_Range_normal = (paste0(round(normal_minimum, digits = 0), ' to ', round(normal_maximum, digits = 0)))) %>%
      dplyr::mutate(data_range_normal = (paste0(min(lubridate::year(all_swe$date_utc), na.rm = TRUE), " to ", max(lubridate::year(all_swe$date_utc), na.rm = TRUE)))) %>%
      dplyr::mutate(normal_datarange_estimated = unique(all_swe$numberofyears_estimated_80, na.rm = TRUE)[!is.na(unique(all_swe$numberofyears_estimated_80, na.rm = TRUE))]) %>%
      dplyr::mutate(normal_datarange_raw = unique(all_swe$numberofyears_80_raw, na.rm = TRUE)[!is.na(unique(all_swe$numberofyears_80_raw, na.rm = TRUE))])

    # get the day of the max and min!! Use only 'real', non estimated data
    min_date <- all_swe %>%
      dplyr::group_by(id, m_d) %>%
      dplyr::slice(which.min(values_stats)) %>%
      dplyr::select(date_utc, id, m_d) %>%
      dplyr::rename(date_min_normal_utc = date_utc)

    max_date <- all_swe %>%
      dplyr::group_by(id, m_d) %>%
      dplyr::slice(which.max(values_stats)) %>%
      dplyr::select(date_utc, id, m_d) %>%
      dplyr::rename(date_max_normal_utc = date_utc)

    # append to data
    dates <- dplyr::full_join(min_date, max_date, by = c("id", "m_d"))
    df_normals_out <- dplyr::full_join(df_normals, dates, by = c("id", "m_d")) %>%
      dplyr::mutate(initial_normal_range = paste0(normal_min, " to ", normal_max))

    # Smooth all the statistics by the 5 -day average?

    # If there is less than 10 years of data available even after trying adjacent sites, return
  } else if (numberofyears_80_raw < 10) {
    df_normals_out <- df_normals_out
  } else {
    df_normals_out <- data.frame("id"  = stn_id,
                               "m_d" = NA,
                               "normal_minimum" = NA,
                               "normal_swe_mean" = NA,
                               "normal_Q5" = NA,
                               "normal_Q10" = NA,
                               "normal_Q25"  = NA,
                               "normal_Q50" = NA,
                               "normal_Q75" = NA,
                               "normal_Q90" = NA,
                               "normal_maximum" = NA,
                               "data_range_normal" = NA,
                               "initial_normal_range" = paste0(normal_min, " to ", normal_max),
                               "normal_datarange_estimated" = NA,
                               "normal_datarange_raw" = NA,
                               "date_min_normal_utc" = NA,
                               "date_max_normal_utc"  = NA,
                               "data_flag" = NA)

  }
  return(df_normals_out)
}
