# -------------------------------------
#' Function for calculating normals for sites that were converted from manual to aswe sites
#' June 2022, Ashlee Jollymore
#' @param id station that you are calculating statistics for
#' @param normal_max maximum year of normal range
#' @param normal_min minimum year of normal range
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_2aswe <- function(id, normal_max, normal_min) {

  # Check to see if the station had a manual station
  if (substring(unique(id), 1, 4) %in% bcsnowdata::snow_manual_location()$LOCATION_ID) {

    manual_id <- substring(unique(id), 1, 4)

    # get manual station data
    manual <- bcsnowdata::get_manual_swe(station_id = manual_id)

    manual_daily <- manual %>%
      dplyr::select(date_utc, swe_mm, survey_period) %>%
      dplyr::rename(Date = date_utc) %>%
      dplyr::mutate(station_type = "manual")

    # get the first and last year of manual data
    man_d_min <- lubridate::year(min(manual_daily$Date))
    man_d_max <- lubridate::year(max(manual_daily$Date))

    man_years <- man_d_max - man_d_min

    # Get automated station data
    aswe_daily <- bcsnowdata::get_aswe_databc(station_id = id,
                                              parameter = "swe",
                                              timestep = "daily") %>%
      dplyr::mutate(Date = as.Date(date_utc)) %>%
      dplyr::rename(swe_mm = value) %>%
      dplyr::mutate(station_type = "aswe") %>%
      dplyr::arrange(Date)

    # get the first and last year of data
    aswe_d_min <- lubridate::year(min(aswe_daily$Date))
    aswe_d_max <- lubridate::year(max(aswe_daily$Date))

    # Number of years of data
    aswe_years <- aswe_d_max - aswe_d_min

    all <- dplyr::full_join(manual_daily, aswe_daily) %>%
      dplyr::select(Date, swe_mm, station_type)

    # Plot the two together as time series
    #plot_a <- ggplot(data = all, aes(x = Date, y = swe_mm, colour = station_type)) +
    #  geom_point() +
    #  ggtitle(paste0("ASWE and Manual Timeseries: ", id)) +
    #  ylab("SWE (mm)") +
    #  xlab("Date")

    # Plot one against each other
    all_xy <- dplyr::full_join(manual_daily %>%
                          dplyr::rename(manual_swe = swe_mm) %>%
                          dplyr::select(-station_type),
                        aswe_daily %>%
                          dplyr::rename(aswe_swe = swe_mm) %>%
                          dplyr::select(-station_type)) %>%
      dplyr::filter(!is.na(aswe_swe), !is.na(manual_swe)) %>%
      dplyr::mutate(month = as.character(lubridate::month(Date)))

    #plot_b <- ggplot(data = all_xy, aes(x = manual_swe, y = aswe_swe)) +
    #  geom_point(colour = all_xy$month) +
    #  geom_smooth(method = "lm", se = FALSE) +
    #  ggtitle(paste0("ASWE vs. Manual: ", id)) +
    #  ylab("ASWE SWE (mm)") +
    #  xlab("Manual SWE (mm)")

    # Check to see how many years of overlap
    # If there is overlap in data, run regression. Otherwise, do not calculate normals
    if (dim(all_xy)[1] > 0) {

      # Get regression statistics
      linear <- summary(lm(aswe_swe ~ manual_swe, data = all_xy))$coefficients

      # get numbers of years of overlap
      years_overlap <- max(lubridate::year(all_xy$Date)) - min(lubridate::year(all_xy$Date))

      if (years_overlap >= 4) {
        missing_aswe <- dplyr::full_join(manual_daily %>%
                                  dplyr::rename(manual_swe = swe_mm) %>%
                                  dplyr::select(-station_type),
                                aswe_daily %>%
                                  dplyr::rename(aswe_swe = swe_mm) %>%
                                  dplyr::select(-station_type)) %>%
          dplyr::filter(!is.na(manual_swe)) %>%
          dplyr::mutate(est_aswe = linear[1] + linear[2] * manual_swe) %>%
          dplyr::mutate(average_diff_percent = mean((aswe_swe - est_aswe)/aswe_swe * 100, na.rm = TRUE)) %>%
          dplyr::mutate(id = id,
                      number_obs = length(na.omit(aswe_swe)),
                      rmse = sqrt(sum((est_aswe - aswe_swe) ^ 2, na.rm = TRUE) / length(na.omit(aswe_swe)))
          )

        # Select only the specific columns you need
        missing_aswe_s <- missing_aswe %>%
          dplyr::mutate(data_flag = ifelse(is.na(aswe_swe), "estimated", "raw")) %>%
          dplyr::mutate(swe_out = ifelse(is.na(aswe_swe), est_aswe, aswe_swe)) %>%
          dplyr::select(Date, survey_period, swe_out, data_flag)

      } else {
        missing_aswe_s <- dplyr::full_join(manual_daily %>%
                                             dplyr::rename(manual_swe = swe_mm) %>%
                                             dplyr::select(-station_type),
                                           aswe_daily %>%
                                             dplyr::rename(aswe_swe = swe_mm) %>%
                                             dplyr::select(-station_type)) %>%
          dplyr::filter(!is.na(manual_swe)) %>%
          dplyr::mutate(data_flag = ifelse(is.na(aswe_swe), "estimated", "raw")) %>%
          dplyr::mutate(swe_out = ifelse(is.na(aswe_swe), est_aswe, aswe_swe)) %>%
          dplyr::select(Date, survey_period, swe_out, data_flag)
      }

      # Check to see if data filling created at record of at least 10 years.
      years_filled <- missing_aswe_s %>%
        dplyr::group_by(survey_period) %>%
        dplyr::full_join(missing_aswe_s) %>%
        # filter by the normal range
        dplyr::mutate(wr = bcsnowdata::wtr_yr(Date)) %>%
        dplyr::filter(wr <= normal_max, wr >= normal_min) %>% # Filter by the normal dates that you specify
        dplyr::group_by(survey_period) %>%
        dplyr::filter(!is.na(swe_out))

      # Calculate a normal for the survey period with at least 10 years of data
      years_filled_10 <- years_filled %>%
        dplyr::group_by(survey_period) %>%
        dplyr::summarize(number_years = length(swe_out)) %>%
        dplyr::full_join(years_filled) %>%
        dplyr::filter(number_years >= 10) %>%
        dplyr::group_by(survey_period) %>%
        dplyr::mutate(data_range_normal = paste0((min(lubridate::year(years_filled_10$Date), na.rm = TRUE)), " to ", (max(lubridate::year(years_filled_10$Date), na.rm = TRUE))))

      # Calculate the normal statistics for each day of the year
      df_normals <- do.call(data.frame,
                            list(dplyr::summarise(years_filled_10, normal_minimum = min(swe_out, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_swe_mean = mean(swe_out, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_Q5 = quantile(swe_out, 0.05, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_Q10 = quantile(swe_out, 0.1, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_Q25 = quantile(swe_out, 0.25, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_Q50 = quantile(swe_out, 0.5, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_Q75 = quantile(swe_out, 0.75, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_Q90 = quantile(swe_out, 0.90, na.rm = TRUE), .groups = "keep"),
                                 dplyr::summarise(years_filled_10, normal_maximum = max(swe_out, na.rm = TRUE), .groups = "keep"))) %>%
        dplyr::select(-survey_period.1, -survey_period.2, -survey_period.3, -survey_period.4, -survey_period.5, -survey_period.6, -survey_period.7, -survey_period.8) %>%
        #dplyr::mutate(Data_Range_normal = (paste0(round(normal_minimum, digits = 0), ' to ', round(normal_maximum, digits = 0)))) %>%
        dplyr::mutate(initial_normal_range = (paste0(normal_min, " to ", normal_max))) %>%
        dplyr::mutate(normal_datarange_raw = aswe_d_max - aswe_d_min) %>%
        dplyr::full_join(years_filled_10 %>% dplyr::select(survey_period, number_years, data_range_normal)) %>%
        dplyr::rename(normal_datarange_estimated = number_years) %>%
        dplyr::mutate(id = id) %>%
        unique() %>%
        dplyr::mutate(data_flag = "<10 years data; filled from manual data") %>%
        dplyr::mutate(m_d = format(strptime(format(paste0(survey_period, "-2022"), format = "%d-%b-%Y"), format = "%d-%b-%Y"), format = "%m-%d"))

      # get the day of the max and min!! Use only 'real', non estimated data
      min_date <- years_filled_10 %>%
        dplyr::group_by(survey_period) %>%
        dplyr::slice(which.min(swe_out)) %>%
        dplyr::select(Date, survey_period) %>%
        dplyr::rename(date_min_normal_utc = Date)

      max_date <- years_filled_10 %>%
        dplyr::group_by(survey_period) %>%
        dplyr::slice(which.max(swe_out)) %>%
        dplyr::select(Date, survey_period) %>%
        dplyr::rename(date_max_normal_utc = Date)

      dates <- dplyr::full_join(min_date, max_date)
      df_normals_out <- dplyr::full_join(df_normals, dates)
    } else {
      df_normals_out <- NA
    }
  } else {
    df_normals_out <- NA
  }
}


