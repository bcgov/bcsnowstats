# Function for calculating basin averaged SWE
# Used for plotting teh basin averaged SWE - both interactive and the static plots

# ================================
# Function for retrieving the basin data for the sites you specify - to be used internally
# ================================
basin_sites <- function(site_input, ...){
  # Get data
  test_1 <- bcsnowdata::get_aswe_databc(station_id = site_input,
                            get_year = "All",
                            use_archive = "Yes",
                            update_archive = "No",
                            directory_archive = "C:/Users/AJOLLYMO/RProjects/SnowData_archive/cache/",
                            parameter_id = "SWE")

  # get water year
  test_1$wr <- bcsnowdata::wtr_yr(dates = test_1$date_utc)
  #data_aswe <- percentiles_ASWE(archive = archive, data_SWE_1 = data_SWE_1, station_id = ASWE_sites, survey_period = "All", get_year = "All")

  # get the mean SWE by day, rather than choosing just the 14:00 measurement
  df_tmp_1 <- test_1 %>%
    #dplyr::filter(lubridate::hour(date_utc) == 16 | lubridate::hour(date_utc) == 15 |lubridate::hour(date_utc) == 14 | lubridate::hour(date_utc) == 17) %>% # get only 16:00 or 15:00 daily measurement.
    dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))  %>%
    dplyr::mutate(Date_dmy = as.Date(date_utc)) %>%
    #dplyr::mutate(Date_dmy = dmy(as.Date(date_utc))) %>%
    dplyr::group_by(Date_dmy) %>%
    dplyr::mutate(mean_day = mean(value, na.rm = TRUE)) #%>%

  # get a SWE measurement by day
  df_tmp_stat <- df_tmp_1 %>%
    dplyr::distinct(Date_dmy, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(m_d)

  # Calculate statistics through function. return a table of statistics for each day of the year
  df_stat <- snow_stats(data = df_tmp_stat, normal_min = 1981, normal_max = 2010, data_ID = "mean_day")

  # bind statistics to this year's data
  data_currentwr <- df_tmp_stat %>%
    dplyr::filter(wr == bcsnowdata::wtr_yr(Sys.Date()))

  # Bind with statistics
  data_plot <- full_join(df_stat, data_currentwr, by = "m_d", .keep_all = TRUE) %>%
    dplyr::select(date_utc, m_d, value, MIN, Q25, Q50, Q75, MAX) %>%
    dplyr::mutate(station_id = site_input)

  # Bind statistics to the daily mean across all years - historic data
  dailymean_stats <- full_join(df_stat, df_tmp_stat, by = c("m_d", "station_id"), .keep_all = TRUE) %>%
    dplyr::select(date_utc, m_d, wr, mean_day, MIN, Q25, Q50, Q75, MAX) %>%
    dplyr::arrange(date_utc) %>%
    dplyr::mutate(station_id = site_input)

  # find when Data_UTC is NA in current water year
  uniques <- data_plot[!is.na(data_plot$date_utc),]
  na.authId <- which(is.na(data_plot$date_utc))
  na.sessionId <- data_plot$m_d[na.authId]

  data_plot$month <- as.numeric(stringr::str_split_fixed(data_plot$m_d, "-", 2)[,1])

  # Assign a date column. If the month is <=10, then the year is the current year
  data_plot <- data_plot %>%
    dplyr::mutate(Date = ifelse(month >= 10,
                                paste0(m_d, "-", year(Sys.Date())),
                                 paste0(m_d, "-", year(Sys.Date())+1))) %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m-%d-%Y"))

  data_plot[na.authId,]$date_utc <- data_plot[na.authId,]$Date

  return(data_plot)
}

# Function for getting the basin avergaed SWE

Basin_averaged_SWE <- function(basin_input, exceptions, survey_period, get_year, ...){

  # Get a list of sites
  sites_first <- basin_name(id = "All", basin_input, exceptions = exceptions)

  # Select the basin that you want to calculate the averaged SWE for
  sites_basin <- sites_first %>%
    dplyr::filter(Basin == basin_input)

  all_sites <- paste(sites_basin$station_id_used, collapse = ";")
  all_sites_1 <- unlist(strsplit(as.character(all_sites), ";"))
  all_sites_2 <- gsub("\t","",all_sites_1)
  all_sites_3 <- unique(gsub(" ","",all_sites_2))

  # Filter by ASWE and manual
  ASWE_sites <- all_sites_3[all_sites_3 %in% snow_auto_location()$LOCATION_ID]

  # List of manual sites
  manual_sites <- all_sites_3[all_sites_3 %in% snow_manual_location()$LOCATION_ID]
  #if(length(ASWE_sites) >0){

  # Run function to retieve data for the ASWE sites
  all_sites_stats <- lapply(ASWE_sites, basin_sites)

  # Unfold the data you just retrieved and
  all_sites_statsunqind <- tibble::as_tibble(do.call("rbind", all_sites_stats)) %>%
    dplyr::mutate(d_m_y = format.Date(date_utc, "%d-%m-%Y")) %>%
    dplyr::mutate(d_m_y = as.Date(d_m_y, format = "%d-%m-%Y")) %>%
    dplyr::mutate(wr = bcsnowdata::wtr_yr(d_m_y))

  # take the average of all of the values by day across all sites. First take daily average by site, then take the average across all sites
  SBI_average <- all_sites_statsunqind %>%
    dplyr::mutate(d_m_y = format.Date(date_utc, "%d-%m-%Y")) %>%
    dplyr::mutate(d_m_y = as.Date(d_m_y, format = "%d-%m-%Y")) %>%
    dplyr::group_by(station_id, d_m_y) %>%
    dplyr::summarise_at(c("value", "MIN", "Q25", "Q50", "Q75", "MAX"), funs(mean(., na.rm=TRUE))) %>%
    dplyr::rename(SWE = value) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(d_m_y) %>%
    dplyr::summarise_at(c("SWE", "MIN", "Q25", "Q50", "Q75", "MAX"), funs(mean(., na.rm=TRUE))) %>%
    reshape2::melt(id = "d_m_y")
}

# =====================
#' Function for getting the daily averaged SWE for interactive plots.
#' Also includes historic years to include on plotly graph
#' Note that this function returns DAILY AVERAAGE Swe values, rather than hourly ones (as does the function above)
#' @param basin_input basin name
#' @param exceptions sites to leave out
#' @export
#' @keywords internal
#' @examples \dontrun{}

basin_averaged_swe_interactive <- function(basin_input, exceptions){

  # Get a list of sites
  sites_basin <- basin_name(id = "All", basin = basin_input, exceptions = exceptions)

  # Select the basin that you want to calculate the averaged SWE for
  all_sites <- paste(sites_basin$Station_ID_used, collapse = ";")
  all_sites_1 <- unlist(strsplit(as.character(all_sites), ";"))
  all_sites_2 <- gsub("\t","",all_sites_1)
  all_sites_3 <- unique(gsub(" ","",all_sites_2))

  # Filter by ASWE and manual
  ASWE_sites <- all_sites_3[all_sites_3 %in% bcsnowdata::snow_auto_location()$LOCATION_ID]

  # List of manual sites
  manual_sites <- all_sites_3[all_sites_3 %in% bcsnowdata::snow_manual_location()$LOCATION_ID]
  #if(length(ASWE_sites) >0){

  basin_sites <- function(site_input){

    test_1 <- bcsnowdata::get_aswe_databc(station_id = site_input,
                              get_year = "All",
                              parameter_id = "SWE")

    # get water year
    test_1$wr <- bcsnowdata::wtr_yr(dates = test_1$date_utc )

    # get the mean SWE by day, rather than choosing just the 14:00 measurement
    df_tmp_stat <- test_1 %>%
      #dplyr::filter(lubridate::hour(date_utc) == 16 | lubridate::hour(date_utc) == 15 |lubridate::hour(date_utc) == 14 | lubridate::hour(date_utc) == 17) %>% # get only 16:00 or 15:00 daily measurement.
      dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))  %>%
      dplyr::mutate(Date_dmy = as.Date(date_utc)) %>%
      #dplyr::mutate(Date_dmy = dmy(as.Date(date_utc))) %>%
      dplyr::group_by(Date_dmy) %>%
      dplyr::mutate(mean_day = mean(value, na.rm = TRUE)) %>%
      dplyr::distinct(Date_dmy, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(m_d)

    # insert NA values for missing part of the year
    Date_dmy <- data.frame(seq(as.Date(paste0(bcsnowdata::wtr_yr(Sys.Date()), "-01-01")), as.Date(paste0(bcsnowdata::wtr_yr(Sys.Date()), "-12-31")), "day"))
    colnames(Date_dmy) <- "Date_dmy"

    # bind to the data to find when NA
    df_out <- dplyr::full_join(df_tmp_stat, Date_dmy, by = "Date_dmy")

    # return the stats table for averaging
    return(df_out)
  }

  all_sites_stats <- lapply(ASWE_sites, basin_sites)

  all_sites_statsunqind <- tibble::as_tibble(do.call("rbind", all_sites_stats))

  # if there are statistics for sites within a basin, get the basin-averaged SWE
  if (dim(all_sites_statsunqind)[1] > 1) {
   all_sites_statsunqind <- all_sites_statsunqind %>%
    dplyr::mutate(d_m_y = format.Date(date_utc, "%d-%m-%Y")) %>%
    dplyr::mutate(d_m_y = as.Date(d_m_y, format = "%d-%m-%Y"))

   # take the average of all of the values by day across all sites. First take daily average by site, then take the average across all sites
   SBI_average <- all_sites_statsunqind %>%
     #dplyr::rename(value = "mean_day") %>%
     dplyr::ungroup() %>%
     dplyr::group_by(d_m_y) %>%
     dplyr::mutate(mean_basin = mean(mean_day, na.rm=TRUE)) %>% # take the mean value per day across all sites within a basin
     dplyr::select(-mean_day, -station_id, -station_name) %>%
     dplyr::distinct(d_m_y, .keep_all = TRUE) %>%
     dplyr::mutate(date_utc = d_m_y, mean_day = mean_basin, value = mean_basin) %>%
     dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc), station_id = basin_input) %>%
     dplyr::mutate(m_d = format.Date(date_utc, "%m-%d"))

   # Take the statistics for the mean SWE value across all sites within a basin
   mean_stats <- snow_stats(data = SBI_average, normal_min = 1991, normal_max = 2020, data_id = "mean_day")

   # Bind stats to the SBI average for all sites across a basin
   mean_basin_all <- dplyr::full_join(SBI_average, mean_stats, by = c("m_d", "station_id"), .keep_all = TRUE) %>%
     dplyr::ungroup() %>%
     dplyr::select(-mean_day, -value, -d_m_y) %>%
     dplyr::rename(mean_basin_SWE = mean_basin) %>%
     dplyr::mutate(sites_used = toString(ASWE_sites))
   } else {
     # If there are no ASWE sites with data within the basin, return an empty dataframe
     mean_basin_all <- tibble::enframe(basin_input, NaN) %>%
       dplyr::rename(Basin = value)
  }
}
