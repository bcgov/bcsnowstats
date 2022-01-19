#' Function for filling data from missing manual stations by adjacent stations using multiple linear regression model
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param data data that you are filling in data within
#' @param normal_max Max year of the normal time span
#' @param normal_min Min year of the normal time span
#' @param survey_periods_20 survey periods with less than 20 years of data within the nomral time period defined
#' @param num_obs number of observations
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_datafill <- function(data, normal_max, normal_min, survey_periods_20, num_obs, ...) {

 # Use adjacent stations to fill in missing data to bring number of years to 20-30
 location_station <- bcsnowdata::snow_manual_location() %>%
  dplyr::filter(LOCATION_ID %in% unique(data$station_id))
 location_station <- sf::st_as_sf(location_station)

 # All other sites within the vicinity
 location_all <- bcsnowdata::snow_manual_location()
 location_all <- sf::st_as_sf(location_all) %>%
  dplyr::filter(!(LOCATION_ID %in% unique(data$station_id)))

 # 100 km buffer around the site
 fr_buffer <- sf::st_buffer(location_station, dist = 1e5)

 # Filter for those sites within a 100 km buffer
 manual_100km <- sf::st_filter(location_all, fr_buffer)

 # plot to test and check
 # ggplot() +
  #  geom_sf(data = location_station, color = "orange") +
  #  geom_sf(data = manual_100km, color = "black") +
  #  geom_sf(data = fr_buffer, fill = NA)

 # If there are manual sites within a 100 km radius, the, proceed with the normal ratio method to backfill missing data
 if (length(unique(manual_100km$LOCATION_ID)) > 0) {

   # Get data for the sites within 100km
   stations_adj <- unique(manual_100km$LOCATION_ID)

   # Find those stations that have long term data for the survey dates that have 10-20 years of data
   data_adj <- bcsnowdata::get_manual_swe(
      station_id = stations_adj,
      get_year = "All",
      force = FALSE,
      ask = FALSE) %>%
     dplyr::mutate(wr = bcsnowdata::wtr_yr(dates = date_utc)) %>%
     dplyr::filter(wr <= normal_max & wr >= normal_min) %>% # Filter by the normal dates that you specify
     # dplyr::filter(Survey_Period %in% survey_periods_20$Survey_Period) %>% # Filter by the survey period that has 10-20 years of data
     dplyr::group_by(station_id, survey_period) %>%
    # dplyr::mutate(percent_available = length(swe_mm)/30*100) %>% # if there is 100% data, should have 30 measurements
     dplyr::mutate(number_of_observations = length(swe_mm)) %>%
     dplyr::filter(number_of_observations >= 20) # filter only for those sites that have more than 20 years of data for the survey period you need

   # Find the correlation between the station of interest and adjacent stations with
   data_adj_select <- data_adj %>%
     dplyr::select(station_id, survey_period, swe_mm, date_utc, wr) %>%
     dplyr::rename(swe_adj = swe_mm, station_id_adj = station_id) %>%
     dplyr::mutate(survey_period_year = paste0(survey_period, "-", wr))

   # If there is data from adjacent sites within the normal range, use it to fill in data.
   if (dim(data_adj_select)[1] > 0) {
    data_adj_cast <- reshape::cast(data_adj_select, survey_period_year ~ station_id_adj, fun.aggregate = sum, value = c("swe_adj"))

    df_normal_time_select <- data %>%
      dplyr::mutate(survey_period_year = paste0(survey_period, "-", wr)) %>%
      dplyr::select(station_id, survey_period, values_stats, date_utc, survey_period_year)

    data_all <- dplyr::full_join(df_normal_time_select, data_adj_cast, by = c("survey_period_year")) %>%
      dplyr::ungroup() %>%
      dplyr::select(-survey_period_year, -m_d, -station_id, -survey_period, -date_utc)

    # Use multiple linear regression model to predict the SWE for the station you are looking for
    lm1 <- lm(values_stats ~ ., data = data_all)
    #summary(lm1)$coefficients[1]
    #length(summary(lm1)$coefficients[1])

    #Remove stations that are not significantly correlated.
    cof <- data.frame(summary(lm1)$coefficients) %>%
      dplyr::rename(pr = "Pr...t..") %>%
      dplyr::mutate(pr = as.numeric(pr)) %>%
      dplyr::filter(pr  < 0.05)

    cof$station <- gsub("[[:punct:]]", "", row.names(cof))

    if ("Intercept" %in% cof$station) {
      cof <- cof %>%
        dplyr::filter(station != "Intercept")
    }

    if (dim(cof)[1] == 0) {

      # If there are no manual sites within 100 km with data within the normal period significantly coorlated to the station of interes, do not do any data filling
      all_swe_1 <- data %>%
        dplyr::mutate(numberofyears_estimated = numberofyears_raw) %>%
        dplyr::mutate(swe_fornormal = values_stats)

    } else {

     #Remove the stations that are not significantly correlated with the SWE at the station that
     data_accum_sig <- data_all[, c("values_stats", cof$station)]

     lm_sig <- lm(values_stats ~ ., data = data_accum_sig)

     # Extract estimated coefficients to make model
     data_all_s <- dplyr::full_join(df_normal_time_select, data_adj_cast[, c("survey_period_year", gsub("[[:punct:]]", "", row.names(cof)))], by = c("survey_period_year")) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(survey_period_year) %>%
      dplyr::mutate(survey_period = ifelse(!is.na(survey_period), survey_period, substr(survey_period_year, 1, 6))) %>%
      dplyr::filter(survey_period %in% survey_periods_20$survey_period) #Sort by only the survey periods that have 10-20 years of data

     # Create a simulated SWE value from correlation relationships.
     data_all_s$predicted <- predict(lm_sig, newdata = data_all_s)

     # fill in data only from the survey periods with 10-20 years of data
     # Get only the survey periods where there is between 10-20 measurements
     incomplete <- data %>%
      dplyr::filter(survey_period %in% survey_periods_20$survey_period) %>%
      dplyr::mutate(survey_period_year = paste0(survey_period, "-", wr)) %>%
      dplyr::arrange(survey_period_year)

     # Join with the predicted SWE
     all_swe_p <- dplyr::full_join(incomplete, data_all_s %>% dplyr::select(survey_period_year, predicted), by = "survey_period_year") %>%
       dplyr::arrange(date_utc) %>%
       dplyr::mutate(swe_fornormal = ifelse(!is.na(values_stats), values_stats, predicted)) %>%
       dplyr::arrange(survey_period_year) %>%
       dplyr::mutate(survey_period = ifelse(!is.na(survey_period), survey_period, substr(survey_period_year, 1, 6)))

     # Get the number of years of raw+predicted data
     num_obs_pred <- all_swe_p %>%
       dplyr::filter(!is.na(swe_fornormal)) %>%
       dplyr::group_by(survey_period) %>%
       dplyr::mutate(percent_available_pred = length(swe_fornormal) / 30 * 100) %>% # if there is 100% data, should have 30 measurements
       dplyr::mutate(numberofyears_estimated = length(swe_fornormal)) %>%
       dplyr::select(survey_period, numberofyears_estimated) %>%
       unique()

     all_swe_p <- dplyr::full_join(all_swe_p, num_obs_pred, by = "survey_period")

     # Bind to the dataframe containing the survey periods with sufficient time
     all_swe <- dplyr::full_join(all_swe_p, data %>%
                          dplyr::filter(!(survey_period %in% survey_periods_20$survey_period))) %>%
       dplyr::mutate(swe_fornormal = ifelse(!is.na(swe_fornormal), swe_fornormal, values_stats))  # fill in the survey periods that have enough data

     # Calculate normal values from the simulated/augmented dataset
     # Only calculate normals for survey periods that now have 20+ years of data (raw + predicted)
     all_swe_1 <- dplyr::full_join(all_swe, num_obs, by = "survey_period") %>%
      #dplyr::rename(numberofyears_raw = number_of_observations) %>%
      dplyr::mutate(numberofyears_raw = ifelse(!is.na(numberofyears_raw.x), numberofyears_raw.x, numberofyears_raw.y)) %>%
      dplyr::select(-numberofyears_raw.x, -numberofyears_raw.y) %>%
      dplyr::mutate(numberofyears_estimated = ifelse(!is.na(numberofyears_estimated), numberofyears_estimated, numberofyears_raw)) %>%
      dplyr::select(-percent_available)

     all_swe_1$snow_course_name <- zoo::na.locf(all_swe_1$snow_course_name, fromLast = TRUE, na.rm = F)
     all_swe_1$station_id <- zoo::na.locf(all_swe_1$station_id, fromLast = TRUE, na.rm = F)
     all_swe_1$elev_metres <- zoo::na.locf(all_swe_1$elev_metres, fromLast = TRUE, na.rm = F)
    }

   } else {
      # If there are no manual sites within 100 km with data within the normal period, do not do any data filling
      all_swe_1 <- data %>%
         dplyr::mutate(numberofyears_estimated = numberofyears_raw) %>%
         dplyr::mutate(swe_fornormal = values_stats)
   }

 } else {
   # If there are no manual sites within 100 km, don't do any datafilling
    all_swe_1 <- data %>%
       dplyr::mutate(numberofyears_estimated = numberofyears_raw) %>%
       dplyr::mutate(swe_fornormal = values_stats)
 }

 return(all_swe_1)
}
