#' function for checking whether a station had a manual site and for using the manual site to extend the dataset back to calculate normals
#' IN DEVELOPMENT
#' @param data data for a aswe site that you want to backfill
#' @keywords gapfill aswe data with manual
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual2aswe <- function(data) {
 # Does the ASWE site have a corresponding MSWE site that can be used to extend the dataset?
 if (unique(data$station_id) %in% bcsnowdata::snow_auto_location()$LOCATION_ID) {

  # Was the site converted from a manual site to a ASWE site? Get the name without the P (P shows that the site is ASWE)
  station_id_manual <- substring(unique(data$station_id), 1, 4)

  if (station_id_manual %in% bcsnowdata::snow_manual_location()$LOCATION_ID) {

    # Get the manual station data
    data_manual <- bcsnowdata::get_manual_swe(station_id = station_id_manual,
                                              survey_period = "All",
                                              get_year = "All",
                                              force = FALSE,
                                              ask = FALSE) %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc))

    # How many years of overlap is there?
    years_overlap <- data %>%
      ungroup() %>%
      dplyr::filter(!is.na(swe_mean)) %>%
      dplyr::filter(wr %in% unique(data_manual$wr)) %>%
      dplyr::select(wr) %>%
      unique()

    if (dim(years_overlap)[1] < 5) {
      # if there is less than 5 years of overlap, don't calculate a normal
      all_swe <- data_20t10 %>%
        dplyr::arrange(date_utc) %>%
        dplyr::mutate(swe_fornormal = swe_mean) %>%
        dplyr::mutate(numberofyears_estimated_80 = numberofyears_80_raw)
    } else {
      # if there is at least 5 years of data, then calculate correlation to extend the ASWE data

      # Plot the two against each other
      ggplot() +
        geom_point(data = data_manual, aes(x = date_utc, y = swe_mm)) +
        geom_point(data = data, aes(x = date_utc, y = swe_mean), colour = "blue")

      # Pull out only the ASWE data that overlaps with the manual sites
      overlap <- full_join(data, data_manual, by = "date_utc") %>%
        dplyr::filter(!is.na(swe_mm)) %>%
        dplyr::filter(!is.na(swe_mean))

      # Plot the manual vs the ASWE
      ggplot() +
        geom_point(data = overlap, aes(x = swe_mm, y = swe_mean))

      # Get linear firs
      m = coef(lm(swe_mean ~ swe_mm, data = overlap))["swe_mm"]
      b = coef(lm(swe_mean ~ swe_mm, data = overlap))["(Intercept)"]

      # Extend the ASWE
      overlap$swe_fornormal <- overlap$swe_mm * m + b

      # plot all three

      ggplot() +
        geom_point(data = overlap, aes(x= date_utc, y = swe_mm)) +
        geom_point(data = overlap, aes(x= date_utc, y = swe_mean), colour = "blue") +
        geom_point(data = overlap, aes(x= date_utc, y = swe_fornormal), colour = "red")

      # error!
       overlap$error = (overlap$swe_fornormal - overlap$swe_mean)/overlap$swe_mean*100

      mean_abs_error <- mean(abs(overlap$error), na.rm = TRUE)

      # If mean absolute error is less than a threshold (10%?), then calculate data. Otherwise, don't fill in
      if (mean_abs_error <= 10) {

        overlap <- full_join(data_20t10, data_manual, by = "date_utc")  %>%
          dplyr::arrange(date_utc)

         all_swe <- data_20t10 %>%

          dplyr::mutate(swe_fornormal = v)
      }

    }

    } else {
     # if there is no manual data, then don't calculate a normal
     all_swe <- data_20t10 %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(swe_fornormal = values_stats) %>%
      dplyr::mutate(numberofyears_estimated_80 = numberofyears_80_raw)
    }
 }
}
