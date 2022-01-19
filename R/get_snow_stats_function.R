#' Function for calculating statistics for a snow station
#' Diverts into either the workflow for calculating ASWE or manual snow station
#' November 2021, Ashlee Jollymore
#' @param station_id ID of the manual station(s) that you want to calculate statistics for
#' @param survey_period Month-Day that you want to calculate statistics for.
#' @param get_year Water year that you want to retrieve data for. Defaults to "All"
#' @param normal_min Water year that the normal date range begins
#' @param normal_max Water year that the normal date range ends
#' @param incorrect_sites Sites that appear to have incorrect data for the survey period and year selected. Defaults to NA
#' @param incorrect_data If there appears to be incorrect sites, then the user can input what the SWE data should be (if catalog is wrong)
#' @export
#' @keywords Retrieve snow statistics
#' @examples \dontrun{}

get_snow_stats <- function(station_id, survey_period, get_year, normal_min, normal_max, force = TRUE) {

  # Check to see whether the station is a manual or automated station
  id_aswe <- station_id[station_id %in% bcsnowdata::snow_auto_location()$LOCATION_ID]

  id_manual <- station_id[station_id %in% bcsnowdata::snow_manual_location()$LOCATION_ID]

  if (length(id_aswe) > 0) {
    df_aswe <- stats_aswe(station_id = id_aswe,
               survey_period = survey_period,
               get_year = get_year,
               normal_min = normal_min,
               normal_max = normal_max,
               force = force)
  }
  # Manual data
  if (length(id_manual) > 0) {
    df_manual <- stats_MSWE(station_id = id_manual,
               survey_period = survey_period,
               get_year = get_year,
               normal_min = normal_min,
               normal_max = normal_max)
  }
  # Bind together
  if (exists("df_aswe") && exists("df_manual")) {
    all <- dplyr::full_join(df_aswe, df_manual)
  } else if (exists("df_aswe") && !exists("df_manual")) {
    all <- df_aswe
  } else if (!exists("df_aswe") && exists("df_manual")) {
    all <- df_manual
  }
}
