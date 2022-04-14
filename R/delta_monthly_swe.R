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

# ============
#' Calculate the change in SWE by month for all of the years on record
#' get data for one station
#' @param month_select month that you are calculating data for
#' @param data historic SWE data for a particular station
#' @export
#' @keywords internal
#' @examples \dontrun{}

SWE_diff_month <- function(month_select, data){

  # Get only the first and last days of the month
  select_month <- data %>%
    dplyr::filter(month == month_select) %>%
    dplyr::filter(day %in% c("1", "01", "28", "29", "30", "31")) %>%
    dplyr::arrange(Date)

  if (dim(select_month)[1] > 0 & max(select_month$day, na.rm = TRUE) %in% c("28", "29", "30", "31")) {

   # Figure out the difference in SWE between the first and last day of the mont
   if (max(select_month$day, na.rm = TRUE) == "31") {
    data_diff <- select_month %>%
      dplyr::filter(day %in% c("1", "01", "31")) %>%
      dplyr::arrange(Date) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
      dplyr::filter(!is.na(first_diff))%>%
      dplyr::select(Date, year, first_diff) %>%
      dplyr::ungroup()

   } else if (max(select_month$day, na.rm = TRUE) == "30"){
    data_diff <- select_month %>%
      dplyr::filter(day %in% c("1", "01", "30")) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
      dplyr::filter(!is.na(first_diff))%>%
      dplyr::select(Date, year, first_diff) %>%
      dplyr::ungroup()
   } else if (max(select_month$day, na.rm = TRUE) == "29" || max(select_month$day, na.rm = TRUE) == "28"){
    # Section for when the max day is 28
    data_diff_28 <- select_month %>%
      dplyr::group_by(year) %>%
      dplyr::filter(max(day) == c("28")) %>%
      dplyr::filter(day %in% c("1", "01", "28")) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
      dplyr::filter(!is.na(first_diff))%>%
      dplyr::select(Date, year, first_diff) %>%
      dplyr::ungroup()
    # Section for when the max day is 29 - leap years
    data_diff_29 <- select_month %>%
      dplyr::group_by(year) %>%
      dplyr::filter(max(day) == c("29")) %>%
      dplyr::filter(day %in% c("1", "01", "29")) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
      dplyr::filter(!is.na(first_diff))%>%
      dplyr::select(Date, year, first_diff) %>%
      dplyr::ungroup()

    # Bind together
    data_diff <- rbind(data_diff_28, data_diff_29) %>%
      dplyr::arrange(Date)

   }

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

   delta_stats <- cbind(unique(select_month$month), mean, median, min, Q5, Q25, Q50, Q75, Q90, max, n_years)
   colnames(delta_stats)[1] <- "Month"

   df_out <- list()

   df_out$statistics <- delta_stats
   df_out$monthlydata <- data_diff

  } else {
    df_out = NULL
  }

  if (exists("df_out")) {
    return(df_out)
  }
}

# ==============
#' Function for calculating the monthly snow accumulation for the current year
#' @param month_select Month that you are calculating the change in SWE for
#' @param data current year SWE data
#' @export
#' @keywords internal
#' @examples \dontrun{}

SWE_diff_month_current <- function(month_select, data){

   # Get only the first and last days of the month
   select_month <- data %>%
      dplyr::filter(month == month_select) %>%
      dplyr::filter(day %in% c("1", "01", "28", "29", "30", "31")) %>%
      dplyr::arrange(Date)

   if (dim(select_month)[1] > 0) {

      # Figure out the difference in SWE between the first and last day of the month
      if (max(select_month$day, na.rm = TRUE) == "31") {
         data_diff <- select_month %>%
            dplyr::filter(day %in% c("1", "01", "31")) %>%
            dplyr::arrange(Date) %>%
            dplyr::group_by(year) %>%
            dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
            dplyr::filter(!is.na(first_diff))%>%
            dplyr::select(Date, year, first_diff) %>%
            dplyr::ungroup()

      } else if (max(select_month$day, na.rm = TRUE) == "30"){
         data_diff <- select_month %>%
            dplyr::filter(day %in% c("1", "01", "30")) %>%
            dplyr::group_by(year) %>%
            dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
            dplyr::filter(!is.na(first_diff))%>%
            dplyr::select(Date, year, first_diff) %>%
            dplyr::ungroup()
      } else if (max(select_month$day, na.rm = TRUE) == "29" || max(select_month$day, na.rm = TRUE) == "28"){
         # Section for when the max day is 28
         data_diff_28 <- select_month %>%
            dplyr::group_by(year) %>%
            dplyr::filter(max(day) == c("28")) %>%
            dplyr::filter(day %in% c("1", "01", "28")) %>%
            dplyr::group_by(year) %>%
            dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
            dplyr::filter(!is.na(first_diff))%>%
            dplyr::select(Date, year, first_diff) %>%
            dplyr::ungroup()
         # Section for when the max day is 29 - leap years
         data_diff_29 <- select_month %>%
            dplyr::group_by(year) %>%
            dplyr::filter(max(day) == c("29")) %>%
            dplyr::filter(day %in% c("1", "01", "29")) %>%
            dplyr::group_by(year) %>%
            dplyr::mutate(first_diff = dailySWE_interp - lag(dailySWE_interp)) %>%
            dplyr::filter(!is.na(first_diff))%>%
            dplyr::select(Date, year, first_diff) %>%
            dplyr::ungroup()

         # Bind together
         data_diff <- rbind(data_diff_28, data_diff_29) %>%
            dplyr::arrange(Date)
      }

   } else {
     data_diff = NULL
   }

   if (exists("data_diff")){
     return(data_diff)
   }
}

# =========================================
#' Function for getting statistics for the monthly delta SWE using historic data
#' December 2021, Ashlee Jollymore
#' @param id station ID that you are calculating box plot statistics for
#' @export
#' @keywords internal
#' @examples \dontrun{}

getmonthly_deltaSWE <- function(id) {

 test_1 <- bcsnowdata::get_aswe_databc(station_id = id,
                          get_year = "All",
                          parameter = "swe",
                          timestep = "daily") # one site, all years

 # If there is any data for the site, run monthly delta SWE statistics
 if (dim(test_1)[1] > 0) {

  # Filter out the current water year so you are only using historic
  month_deltaSWE <- test_1 %>%
   dplyr::mutate(m_y = format(date_utc, "%m-%Y")) %>%
   dplyr::mutate(day = format(date_utc, "%d")) %>%
   dplyr::mutate(Date = as.Date(date_utc)) %>%
   dplyr::mutate(water_year = bcsnowdata::wtr_yr(Date)) %>%
   dplyr::filter(water_year < bcsnowdata::wtr_yr(Sys.Date())) %>%
   dplyr::group_by(Date)

  # Take daily mean SWE
  daily_swe <- dplyr::summarise(month_deltaSWE, Mean_dailySWE = mean(value, na.rm = TRUE)) %>%
   dplyr::mutate(m_y = format(Date, "%m-%Y")) %>%
   dplyr::mutate(day = format(Date, "%d")) %>%
   dplyr::mutate(month = format(Date, "%m")) %>%
   dplyr::mutate(year = format(Date, "%Y")) %>%
   dplyr::filter(!is.na(Mean_dailySWE))

  if (dim(daily_swe)[1] > 1) {

   # Linear interpolation to fill in missing days of data.
   # Create a time series of the snow accomulation months: oct - july
   time_start <- min(daily_swe$Date)
   time_end <- max(daily_swe$Date)

   Date = as.data.frame(seq(as.Date(time_start), as.Date(time_end), by = "day"))
   colnames(Date) <- c('Date')

   # Bind to the daily SWE and perform linear interpolation of missing data
   daily_swe_NA <- dplyr::full_join(daily_swe, Date) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(dailySWE_interp = zoo::na.approx(Mean_dailySWE, na.rm = FALSE))

   watermonths <- c(seq(10,12, by = 1), c("01", "02", "03", "04", "05", "06", "07", "08"))

   watermonths_data <- watermonths[watermonths %in% unique(daily_swe_NA$month, na.rm = TRUE)]

   station_diff <- lapply(watermonths_data,
                          SWE_diff_month,
                          data = daily_swe_NA)

   df_list <- list()

   # unlist to get all of the differences by month
   for (i in 1:length(watermonths_data)) {
      if (length(station_diff[[i]]$statistics) > 0){
       station_diff_unlist <- do.call("cbind.data.frame", station_diff[[i]]$statistics)
       station_diff_unlist$Station_ID <- id
       df_list[[i]] <- station_diff_unlist
      }
   }

   # Unlist statistics
   station_diff_unlist <- do.call("rbind.data.frame", df_list)

   # Unlist the data
   df_list_data <- list()
   # unlist to get all of the differences by month
   for (i in 1:length(watermonths_data)) {
      if (!is.null(dim(station_diff[[i]]$monthlydata)[1]) && dim(station_diff[[i]]$monthlydata)[1] > 0) {
       temp <- do.call("cbind.data.frame", station_diff[[i]]$monthlydata)
       temp$Station_ID <- id
       df_list_data[[i]] <- temp
      }
   }

   # Unlist statistics
   station_data_unlist <- do.call("rbind.data.frame", df_list_data) %>%
      dplyr::mutate(Month = lubridate::month(Date))


  } else {
    print("No data for site")
    station_diff_unlist <- NA
    station_data_unlist <- NA
  }
 } else {
   print("No data for site")
   station_diff_unlist <- NA
   station_data_unlist <- NA
 }

 return(list(station_diff_unlist, station_data_unlist))
}

# =========================================
#' Function for getting statistics for the monthly delta SWE - what the monthly change in SWE has been in this year to date
#' @param id station ID that you are calculating box plot statistics for
#' @export
#' @keywords internal
#' @examples \dontrun{}

getmonthly_deltaSWE_YTD <- function(id) {

   test_1 <- bcsnowdata::get_aswe_databc(station_id = id,
                             get_year = bcsnowdata::wtr_yr(Sys.Date()),
                             parameter = "swe",
                             timestep = "daily") # one site, all years

   # If there is any data for the site, run monthly delta SWE statistics
   if (dim(test_1)[1] > 0) {
    # ===========================================
    # Get the statistics for the current year data - mean snow accumulation by month
    # ===========================================
    current_year <- test_1 %>%
      dplyr::mutate(m_y = format(date_utc, "%m-%Y")) %>%
      dplyr::mutate(day = format(date_utc, "%d")) %>%
      dplyr::mutate(Date = as.Date(date_utc)) %>%
      dplyr::mutate(water_year = bcsnowdata::wtr_yr(Date)) %>%
      dplyr::filter(water_year == bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::group_by(Date)

    # Take daily mean SWE
    if ("value" %in% colnames(current_year)) {
      daily_swe_current <- dplyr::summarise(current_year, Mean_dailySWE = mean(value, na.rm = TRUE)) %>%
        dplyr::mutate(m_y = format(Date, "%m-%Y")) %>%
        dplyr::mutate(day = format(Date, "%d")) %>%
        dplyr::mutate(month = format(Date, "%m")) %>%
        dplyr::mutate(year = format(Date, "%Y")) %>%
        dplyr::filter(!is.na(Mean_dailySWE)) %>%
        dplyr::group_by(month)

      if (dim(daily_swe_current)[1] > 0){
        # Linear interpolation to fill in missing days of data.
        # Create a time series of the snow accomulation months: oct - july
        #time_start <- min(daily_swe_current$Date, na.rm = TRUE)
        time_start <- as.Date(paste0(as.character(bcsnowdata::wtr_yr(Sys.Date())-1),"-", "10", "-", "01"))
        time_end <- max(daily_swe_current$Date, na.rm = TRUE)

        Date = as.data.frame(seq(as.Date(time_start), as.Date(time_end), by = "day"))
        colnames(Date) <- c('Date')

        # Bind to the daily SWE and perform linear interpolation of missing data
        daily_swe_NA <- dplyr::full_join(daily_swe_current, Date) %>%
          dplyr::arrange(Date) %>%
          dplyr::ungroup()
        # if oct 1 is NA, fill with 0
        daily_swe_NA$Mean_dailySWE[1] <- ifelse(is.na(daily_swe_NA$Mean_dailySWE["2019-10-01"]), 0, daily_swe_NA$Mean_dailySWE["2019-10-01"])

        # Perform interpolation
        daily_swe_NA <- daily_swe_NA %>%
          dplyr::arrange(Date) %>%
          dplyr::mutate(m_y = format(Date, "%m-%Y")) %>%
          dplyr::mutate(day = format(Date, "%d")) %>%
          dplyr::mutate(month = format(Date, "%m")) %>%
          dplyr::mutate(year = format(Date, "%Y")) %>%
          dplyr::mutate(dailySWE_interp = zoo::na.approx(Mean_dailySWE,
                                                   na.rm = T))

        watermonths <- c(seq(10,12, by = 1), c("01", "02", "03", "04", "05", "06", "07", "08"))

        # Calculate the snow accumulation for each month
        station_diff_current <- lapply(watermonths, SWE_diff_month_current, data = daily_swe_NA)

        # Unlist statistics
        station_diff_current_unlist <- do.call("rbind.data.frame", station_diff_current)

        if (dim(station_diff_current_unlist)[1] > 0) {
          station_diff_current_unlist <- do.call("rbind.data.frame", station_diff_current) %>%
            dplyr::mutate(Month = lubridate::month(Date))
        }
      } else {
        station_diff_current_unlist <- NA
      }
    } else {
      station_diff_current_unlist <- NA
    }
   } else {
     station_diff_current_unlist <- NA
   }

   # Write the year to date monthly accumulation data
   #write.csv(station_diff_current_unlist, file = paste0(path_save, "YTDmonthlydeltaSWE_", id, ".csv"),
  #           row.names = F)

   return(station_diff_current_unlist)
}

# ==================================================================
# Get the median change in monthly swe to plot
# ==================================================================
#time_start <- Sys.time()
#id <- c("2F05P")
#test_1 <- get_aswe_databc(station_id = id,
#                          get_year = "All",
#                          use_archive = "Yes",
#                          update_archive = "No",
#                          directory_archive = "C:/Users/AJOLLYMO/RProjects/SnowData_archive/cache/",
#                          parameter_id = "SWE") # one site, all years
#total_time_getdata <- Sys.time()-time_start

# Function for creating a hypothetical SWE trend based on the delta monthly SWE
# Create a dataframe of SWE values
get_monthly_deltaSWE <- function(data_plot_1, id) {

 monthly_deltaSWE <- tryCatch(read.csv(file = paste0("V:/Real-time_Data/ASP_daily_interactive/data/Statistics/monthlydeltaSWE_", id, ".csv"),
                                      stringsAsFactors = FALSE),
                             error = function(e) NaN)

 if (all(!is.na(monthly_deltaSWE))) {
   # if the monthly change in file exists, make the future plot

   monthly_deltaSWE <- monthly_deltaSWE %>%
    dplyr::arrange(Month) %>%
    dplyr::mutate(Month = as.numeric(Month))

   month_current <- month(Sys.Date())

   if (month_current >= 10) {
     # Filter the delta SWE statistics to be beyond the current month
     monthly_deltaSWE_curr <- monthly_deltaSWE %>%
       dplyr::filter(Month >= month(Sys.Date()))
   } else {
     # Filter the delta SWE statistics to be beyond the current month
     monthly_deltaSWE_curr <- monthly_deltaSWE %>%
       dplyr::filter(Month < 10) %>%
       dplyr::filter(Month >= as.numeric(month(Sys.Date())))
   }

   ## Filter only the positive snow accumulation months - doesn't make sense to include the snow melt
   #monthly_deltaSWE_curr <- monthly_deltaSWE_curr %>%
   #   dplyr::filter(month != c("6","7","8"))

   # Get the SWE measurement on the first of the current month
   day1 <- data_plot_1 %>%
     dplyr::mutate(Date_dmy = as.Date(date_utc)) %>%
     dplyr::filter(Date_dmy == as.Date(paste0("01-", format(Sys.Date(),"%m"), "-", as.character(wtr_yr(Sys.Date()))), "%d-%m-%Y")) %>%
     dplyr::group_by(Date_dmy) %>%
     dplyr::mutate(value = mean(value))
   day1_SWE <- unique(day1$value)

   # How many more months are there left within the snow season?
   snowseason_months <- c(seq(10, 12, by = 1), seq(1:8))
   current_month <- unique(month(day1$Date_dmy))

   # Number of months left in the snow season
   month_season <- match(current_month, snowseason_months)
   months_left <- length(snowseason_months) - as.numeric(month_season)

   # Calculate the hypothetical SWE for the first of the next month
   nextmonth_mean <- monthly_deltaSWE_curr$mean[1] + day1_SWE
   nextmonth_median <- monthly_deltaSWE_curr$median[1] + day1_SWE
   nextmonth_min <- monthly_deltaSWE_curr$MIN[1] + day1_SWE
   nextmonth_max <- monthly_deltaSWE_curr$MAX[1] + day1_SWE
   nextmonth_Q25 <- monthly_deltaSWE_curr$Q25[1] + day1_SWE
   nextmonth_Q75 <- monthly_deltaSWE_curr$Q75[1] + day1_SWE

   next_month_date <- as.Date(paste0("01-0", month(Sys.Date())+1, "-", as.character(wtr_yr(Sys.Date()))), "%d-%m-%Y")

   # Create dataframe
   nextmonth_SWE <- data.frame(cbind(nextmonth_mean, nextmonth_median, nextmonth_min,
                                     nextmonth_max, nextmonth_Q25, nextmonth_Q75))
   colnames(nextmonth_SWE) <- c("mean", "median", "min", "max", "Q25", "Q75")
   nextmonth_SWE$Date <- as.Date(paste0("01-0", month(Sys.Date())+1, "-", as.character(wtr_yr(Sys.Date()))), "%d-%m-%Y")

   # Calculate the hypothetical SWE for the months left within the
   for (i in 1:months_left) {

    # Do the next month
    # Calculate the hypothetical SWE for the first of the next month
    month2_mean <- monthly_deltaSWE_curr$mean[i+1] + nextmonth_SWE$median[i]
    month2_median <- monthly_deltaSWE_curr$median[i+1] + nextmonth_SWE$median[i]
    month2_min <- monthly_deltaSWE_curr$MIN[i+1] + nextmonth_SWE$median[i]
    month2_max <- monthly_deltaSWE_curr$MAX[i+1] + nextmonth_SWE$median[i]
    month2_Q25 <- monthly_deltaSWE_curr$Q25[i+1] + nextmonth_SWE$median[i]
    month2_Q75 <- monthly_deltaSWE_curr$Q75[i+1] + nextmonth_SWE$median[i]

    # Create dataframe
    month2_SWE <- data.frame(cbind(month2_mean, month2_median, month2_min,
                                 month2_max, month2_Q25, month2_Q75))
    colnames(month2_SWE) <- c("mean", "median", "min", "max", "Q25", "Q75")
    month2_SWE$Date <- as.Date(paste0("01-0", month(Sys.Date()) + (i+1), "-", as.character(wtr_yr(Sys.Date()))), "%d-%m-%Y")

    nextmonth_SWE[(i+1), ] <- month2_SWE
   }

   # Find the month that the max median SWE happens
   month_max <- nextmonth_SWE$Date[nextmonth_SWE$median == max(nextmonth_SWE$median)]

   data_out <- nextmonth_SWE %>%
     dplyr::filter(Date <= month_max)

   # Add the last month observed
   data_last_col <- c("mean", "median", "min", "max", "Q25", "Q75")
   data_last_d <- c(day1_SWE, day1_SWE, day1_SWE, day1_SWE, day1_SWE, day1_SWE)
   data_last <- data.frame(rbind(data_last_col, data_last_d))
   data_last <- data_last[2,]
   row.names(data_last) <- NULL
   colnames(data_last) <- data_last_col
   data_last$Date <- as.Date(unique(day1$Date_dmy))

   data_out_list <- list()

   data_out_list[[1]] <- rbind(data_out, data_last) %>%
     dplyr::arrange(Date)

   data_out_list[[2]] <- monthly_deltaSWE

   return(data_out_list)

 } else {
   print("No stats for station")
 }
}

#all_active_ASWE_unlist <- do.call("rbind.data.frame", all_active_ASWE)

#write.csv(all_active_ASWE_unlist, file = "V:/Real-time_Data/ASP_daily_interactive/data/Statistics/monthly_delta_SWE_ASWE.csv")
# Unfold and save all of the stats as a csv file


# Plot the statistics as their own graph - monthly delta SWE

#monthly_deltaSWE <- tryCatch(read.csv(file = paste0("V:/Real-time_Data/ASP_daily_interactive/data/Statistics/monthlydeltaSWE_", id, ".csv"),
#                                      stringsAsFactors = FALSE),
#                             error = function(e) NaN)

# Add an artificial year to make the plot
#monthly_deltaSWE_plot <- monthly_deltaSWE %>%
#   dplyr::mutate(year = ifelse(as.numeric(Month) >= 10, wtr_yr(Sys.Date())-1, wtr_yr(Sys.Date()))) %>%
#   dplyr::mutate(monthyear = paste0("29-", Month,"-", year)) %>%
#   dplyr::mutate(monthyear = as.Date(monthyear, "%d-%m-%Y")) %>%
#   dplyr::arrange(monthyear) %>%
#   dplyr::mutate(Month_num = format(monthyear, "%B"))

#factor(monthly_deltaSWE_plot$Month_num, levels=monthly_deltaSWE_plot$Month_num)

#p <- plot_ly() %>%
#   add_trace(data = monthly_deltaSWE_plot, x = ~factor(monthly_deltaSWE_plot$Month_num, levels=monthly_deltaSWE_plot$Month_num), y = ~mean,
#             type = 'scatter',
#             mode = 'markers+lines',
#             connectgaps = FALSE,
#             color = "Mean",
#             line = list(color = "rgb(130,130,130)",
#                         width = 2, dash = 'dashdot')) %>%
#   add_trace(data = monthly_deltaSWE_plot, x = ~factor(monthly_deltaSWE_plot$Month_num, levels=monthly_deltaSWE_plot$Month_num), y = ~median,
#             type = 'scatter',
#             mode = 'markers+lines',
#             connectgaps = FALSE,
#             color = "Median",
#             line = list(color = "rgb(130,130,130)",
#                         width = 2, dash = 'dashdot')) %>%
#   add_trace(data = monthly_deltaSWE_plot, x = ~factor(monthly_deltaSWE_plot$Month_num, levels=monthly_deltaSWE_plot$Month_num), y = ~MAX,
#             type = 'scatter',
#             mode = 'markers+lines',
#             connectgaps = FALSE,
#             color = "Maximum",
#             line = list(color = "rgb(130,130,130)",
#                         width = 2, dash = 'dashdot')) %>%
#   add_trace(data = monthly_deltaSWE_plot, x = ~factor(monthly_deltaSWE_plot$Month_num, levels=monthly_deltaSWE_plot$Month_num), y = ~MIN,
#             type = 'scatter',
#             mode = 'markers+lines',
#             connectgaps = FALSE,
#             color = "Minimum",
#             line = list(color = "rgb(130,130,130)",
#                         width = 2, dash = 'dashdot')) %>%
#   layout(title = paste0('SWE (mm) for ', station_name, ", ", id),
#          xaxis = list(
#             title = 'Date',
#             type = 'date',
#             range = c(min(d_all_stats$date_utc), max(d_all_stats$date_utc)),
#             tickformat = "%d-%B"),
#          yaxis = list(title = 'SWE (mm)'))



