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

# Plots containing functions for creating interactive snow plots, as well as climate plots
# Started March 2019 - Ashlee Jollymore
# Script contains four functions:
# 1. Function for creating and saving interactive plots for ASWE snow sites, as well as creating interactive plots for the climate and delta SWE plots
# 2. Function for creating and saving basin averaged SWE (FUTURE)
# 3. Function for creating and saving manual (FUTURE)
# Uses the SWE data pipeline created in 2018-2019 by Ashlee Jollymore - all data is gathered and analyzed within the script
#
############################################################

# ================================================
#' Function for getting current year SWE
#' @export
#' @keywords internal
#' @examples \dontrun{}
get_swe <- function(id) {

   # Get statistics data for the site you are plotting
   data_plot_1 <- get_snow_stats(station_id = id,
                                 survey_period = "All",
                                 get_year = "All",
                                 normal_min = 1991,
                                 normal_max = 2020,
                                 force = FALSE)

   # Round all numerics within data to the nearest whole number
   data_plot <- round_df(x = data_plot_1, digits = 0)
}

# ================================================
#' Function for getting current year data
#' @export
#' @keywords internal
#' @examples \dontrun{}
getSWE_all <- function(data){

   # This year's data
   data_plot_2 <- data %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::filter(wr == bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::ungroup() %>%
      dplyr::select("date_utc", "mean_day") %>%
      dplyr::rename(daily_mean = mean_day) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::mutate(d_m = paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc))) %>%
      dplyr::group_by(d_m) %>%
      #dplyr::mutate(daily_mean = round(mean(swe_mm, na.rm = TRUE), digits = 0)) %>% Just use the already calculated mean daily SWE
      dplyr::select(date_utc, daily_mean, d_m) %>%
      dplyr::distinct(daily_mean, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(d_m != "29-2") # remove leap years

   # If there is missing data, insert it
   date_seq <- data.frame(date_utc = seq(as.Date(paste0(wtr_yr(Sys.Date())-1, "-10-01")), as.Date(paste0(wtr_yr(Sys.Date()), "-10-01")), by = "day"))

   data_plot_2_NA <- dplyr::full_join(data_plot_2, date_seq) %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(d_m = paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc)))

   # Append the stats for the entire year
   data_stats <- data %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::ungroup() %>%
      dplyr::select("date_utc",  "min", "max","Q10", "Q25", "Q50", "Q75","Q90", "normal_Q50") %>%
      dplyr::mutate(d_m = paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc))) %>%
      dplyr::distinct(d_m, .keep_all = TRUE) %>%
      dplyr::select(-date_utc)%>%
      dplyr::filter(d_m != "29-2") # remove leap years

   d_all <- dplyr::full_join(data_plot_2_NA, data_stats, by = "d_m") %>%
      dplyr::mutate(Percent_normal = round(daily_mean/normal_Q50 *100, digits = 0))

   # find last date of data
   uniques <- d_all[!is.na(d_all$daily_mean),]
   na.authId <- which(is.na(d_all$date_utc))
   na.sessionId <- d_all$d_m[na.authId]
   year_last <- lubridate::year(d_all$date_utc[min(na.authId)-1])

   # Fill in any NA date values to create an entiere time serious to plot

   # If there is no current year data, you will need to fill in the whole date vector as if there were data for this water year
   if (is.na(na.authId[1])) {
   } else if (na.authId[1] == 1) {
     d_all <- d_all %>%
       dplyr::mutate(month = as.numeric(sapply(strsplit(d_all$d_m, "-"), `[`, 2))) %>%
       dplyr::mutate(year = ifelse(month >=10, bcsnowdata::wtr_yr(Sys.Date())-1, bcsnowdata::wtr_yr(Sys.Date()))) %>% # if the month is oct or greater, assign the previous water year
       dplyr::mutate(date_utc = as.Date(paste0(d_m, "-", year), format = "%d-%m-%Y")) %>%
       dplyr::select(-month, -year) %>%
       dplyr::arrange(date_utc)
   } else if (year_last == bcsnowdata::wtr_yr(Sys.Date())){ # if the water year is the same as this year. If current year less than water year, will need to do something else...
      # fill in with date
      d_all$date_utc[na.authId] <- as.Date(paste0(na.sessionId, "-", year_last), format = "%d-%m-%Y")
   } else if (year_last == bcsnowdata::wtr_yr(Sys.Date()) -1){

      # fill in date
      d_all$month <- as.numeric(sapply(strsplit(d_all$d_m, "-"), `[`, 2))
      d_all$year <- ifelse(d_all$month >=10, year_last, bcsnowdata::wtr_yr(Sys.Date()))
      d_all$date <- as.Date(paste0(d_all$d_m, "-", d_all$year), format = "%d-%m-%Y")

      # Replace NA values with the dates you just created
      date.NA <- which(is.na(d_all$date_utc))[1]

      d_all$date_utc[which(is.na(d_all$date_utc))[1]:length(d_all$date_utc)] <- d_all$date[which(is.na(d_all$date_utc))[1]:length(d_all$date_utc)]

      d_all$date <- NULL
      d_all$month <- NULL
      d_all$year <- NULL

      d_all <- d_all %>%
         dplyr::arrange(date_utc)
   }

   # Rename columns and melt dataframe. Sort by the variable, as well as where it should appear on the graph
   d_all_2 <- d_all %>%
      dplyr::rename(`Current Year` = daily_mean, `Normal (1981-2010)` = normal_Q50, `Median` = Q50) %>%
      dplyr::select(-d_m, -Percent_normal) %>%
      reshape2::melt(id = "date_utc") %>%
      dplyr::mutate(sorting = ifelse(variable == "Current Year", 2, 1)) %>%
      dplyr::arrange(sorting, variable)

   return(d_all_2)
}

# ================================================
#' Function for only getting current year data
#' @export
#' @keywords internal
#' @examples \dontrun{}
getSWE_current <- function(data){
   data_all <- getSWE_all(data)

   # Isolate current year
   # isolate current year
   d_all_curr <- data_all %>%
      dplyr::filter(variable == "Current Year") %>%
      dplyr::mutate(Date = format(date_utc, format = "%d-%b"))

   return(d_all_curr)
}

# ================================================
#' Function for formatting data and plotting. Internal function for use within plot_interactive_function (for looping over multiple sites)
#' @param data_plot_1 entire dataset
#' @param stn_id station ID you are plotting
#' @param save whether to save or not
#' @param path path that you want to save plots in
#' @export
#' @keywords internal
#' @examples \dontrun{}
plot_function <- function(stn_id, data_plot_1, save, path) {

  aswe <- bcsnowdata::snow_auto_location()

  # Get station name(s)
  station_name <- aswe$LOCATION_NAME[aswe$LOCATION_ID %in% as.character(stn_id)]

  df <- data_plot_1 %>%
    dplyr::filter(id == as.character(stn_id)) %>%
    unique()

  # Only run the analysis if there is any data available for the site!
  if (dim(df)[1] > 1) {

    # =================
    # Historic data
    # =================
    data_historic_1 <- df %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::ungroup() %>%
      dplyr::select("date_utc", "mean_day", wr) %>%
      dplyr::rename(daily_mean = mean_day) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::mutate(d_m_y = paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc), "-", lubridate::year(date_utc))) %>%
      dplyr::group_by(d_m_y) %>%
      dplyr::select(date_utc, daily_mean, d_m_y, wr) %>%
      dplyr::distinct(d_m_y, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(year = lubridate::year(date_utc)) %>%
      dplyr::mutate(month = lubridate::month(date_utc))

    # Fill in large gaps in data with NA so that plot doesn't connect lines
    date_historic <- data.frame(as.Date(seq(min(data_historic_1$date_utc), max(data_historic_1$date_utc), by = 'day'))) %>%
      dplyr::rename(date_utc = "as.Date.seq.min.data_historic_1.date_utc...max.data_historic_1.date_utc...")

    d_historic_fill <- merge(data_historic_1, date_historic, all = TRUE) %>%
      dplyr::mutate(year = lubridate::year(date_utc)) %>%
      dplyr::mutate(month = lubridate::month(date_utc))

    # Get the artificial date column to plot against the current year
    d_historic <- d_historic_fill %>%
      dplyr::group_by(wr) %>%
      dplyr::mutate(year_art = ifelse(month >=10, (bcsnowdata::wtr_yr(Sys.Date())-1), bcsnowdata::wtr_yr(Sys.Date()))) %>%
      dplyr::mutate(Date_art = as.Date(paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc), "-", year_art),
                                       format = "%d-%m-%Y")) %>%
      dplyr::select(-year_art, -d_m_y, -month) %>%
      dplyr::mutate(d_m = paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc))) %>%
      dplyr::filter(d_m != "29-2") %>%  # remove leap years
      dplyr::ungroup() %>%
      dplyr::rename(swe_mm = daily_mean) %>%
      dplyr::select(-d_m) %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::filter(wr < bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::mutate(wr = as.factor(wr)) %>%
      dplyr::mutate(year = as.factor(year)) %>%
      dplyr::mutate(Date = format(date_utc, format = "%d-%b")) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::mutate(Date_art = as.Date(Date_art)) %>%
      dplyr::arrange(desc(wr), date_utc) %>% # attempt to re-arrrange plots so that current years are first
      dplyr::mutate(wr = factor(wr, levels = rev(levels(wr)))) %>% # reverse the order of years to have the most recent at the top
      dplyr::mutate(wr_toplot = paste0("WY = ", wr))

    # =================
    # Calculate statistics for current year data
    # =================
    d_all_2 <- getSWE_all(data = df)

    # =======================
    # Smooth statistics with 5-day average
    # =======================
    # if the normal exists for stations with greater than 10 years of data. Lots of data means easier to calculate the stats
    if (all(!is.na(d_all_2[d_all_2$variable == "Normal (1981-2010)",]$value)) && all(!is.null(d_all_2[d_all_2$variable == "Normal (1981-2010)",]$value))) {

      # isolate stats and smooth with five day average
      d_all_stats <- d_all_2 %>%
        dplyr::filter(variable != 'Current Year')  %>%
        dplyr::filter(!is.na(date_utc)) %>% # get rid of leap year data
        dplyr::arrange(variable, date_utc) %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(value_5 = zoo::rollapply(data = zoo::na.approx(value), # fill in missing data within the stats. Use na.approx to approximate NA values
                                               width = 5,
                                               FUN = mean,
                                               align = "center",
                                               fill = NA,
                                               na.rm = T)) %>%
        dplyr::mutate(Date = format(date_utc, format = "%d-%b"))
    } else if (all(is.na(d_all_2[d_all_2$variable == "Normal (1981-2010)",]$value)) || all(is.null(d_all_2[d_all_2$variable == "Normal (1981-2010)",]$value))) {

      # Isolate stats and smooth with five day average
      d_all_stats_t <- d_all_2 %>%
        dplyr::filter(variable != "Current Year" & variable != "Normal (1981-2010)") %>% # remove the na normal variable for stations with less that 10 years of data
        dplyr::filter(!is.na(date_utc)) %>% # get rid of leap year data
        dplyr::arrange(variable, date_utc) %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(Date = format(date_utc, format = "%d-%b"))

      firstNonNA <- min(which(!is.na(d_all_stats_t$value))) # first instance of NA data. Works only for the first varaible, but this trend is likely the same for all variables.

      # If there is greater than 3 days missing at the beginning of each of the variables, need to remove leading NA values in order for 5-day moving average to work.
      if (firstNonNA > 3 && !is.infinite(firstNonNA)) {

        dftmp <- list()
        for (l in 1:length(unique(d_all_stats_t$variable))) {

          d_temp <- subset(d_all_stats_t, variable == unique(d_all_stats_t$variable)[l])

          # First non-na value
          firstNonNA <- min(which(!is.na(d_temp$value)))

          # Look for instances of multiple NA values within the dataframe - trail of NA values at the end of the data
          lastNA <- max(which(is.na(d_temp$value)))

          # if the last value is NA, trim the NA values from the end
          if (lastNA == dim(d_temp)[1]) {
            NAs <- data.frame(Location = which(is.na(d_temp$value))) %>%
              # Change
              purrr::map_df(rev) %>%
              dplyr::mutate(difference = c(NA, diff(Location)))

            # First NA value of the tail of NA
            first_na <- NAs$Location[(which(NAs$difference != -1)-1)[1]]

            # trim the NA values from the end
            d_temp_trim <- d_temp[c(-(first_na:lastNA)),]
          } else {
            d_temp_trim <- d_temp
          }

          dftmp[[l]] <- d_temp_trim[c(-(1:firstNonNA-1)),] %>%
            dplyr::select(date_utc, variable, value) %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(value_5 = zoo::rollapply(data = zoo::na.approx(value), # fill in missing data within the stats
                                                   width = 5,
                                                   FUN = mean,
                                                   align = "center",
                                                   fill = NA,
                                                   na.rm = T,
                                                   partial = TRUE)) %>%
            dplyr::mutate(Date = format(date_utc, format = "%d-%b"))
        }
        roll_mean_data <- do.call("rbind.data.frame", dftmp)
        # Merge the five day mean with the statistics
        d_all_stats <- dplyr::full_join(d_all_2, roll_mean_data, by = c("date_utc", "variable", "value"))
      } else {

        # find only the variables without any entries. Filter out those that are all na (normals!)
        d_check <- d_all_2 %>%
          dplyr::group_by(variable) %>%
          dplyr::summarise_each(~(sum(!is.na(.)))) %>%
          dplyr::filter(value != 0) # filter out those variables without any entries

        # isolate stats and smooth with five day average
        d_all_stats <- d_all_2 %>%
          dplyr::filter(variable %in% d_check$variable) %>% # get rid of variables that have all NA values
          dplyr::filter(variable != 'Current Year')  %>% # Get rid of the current year data if it is there
          dplyr::filter(!is.na(date_utc)) %>% # get rid of leap year data
          dplyr::arrange(variable, date_utc) %>% # arrange by varaible and date
          dplyr::group_by(variable) %>%
          dplyr::arrange(variable, date_utc) %>%
          # dplyr::filter(!is.na(value)) %>% # Filter out the Na values.. will this work!
          dplyr::mutate(value_5 = zoo::rollapply(data = zoo::na.approx(value), # fill in missing data within the stats; only fill in a max of 10
                                                 width = 5,
                                                 by=1,
                                                 partial = TRUE,
                                                 FUN = mean,
                                                 align = "center",
                                                 fill = NA,
                                                 na.rm = T)) %>%
          dplyr::mutate(Date = format(date_utc, format = "%d-%b"))
      }
    } else {

      # Isolate stats and smooth with five day average
      d_all_stats_t <- d_all_2 %>%
        dplyr::filter(variable != "Current Year") %>% # remove the na normal variable for stations with less that 10 years of data
        dplyr::filter(!is.na(date_utc)) %>% # get rid of leap year data
        dplyr::arrange(variable, date_utc) %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(Date = format(date_utc, format = "%d-%b"))

      firstNonNA <- min(which(!is.na(d_all_stats_t$value))) # first instance of NA data. Works only for the first varaible, but this trend is liely the same for all variables.

      # If there is greater than 3 days missing at the beginning of each of the variables, need to remove leading NA values in order for 5-day moving average to work.
      if (firstNonNA > 3) {
        dftmp <- list()
        for (l in 1:length(unique(d_all_stats_t$variable))){

          d_temp <- subset(d_all_stats_t, variable == unique(d_all_stats_t$variable)[l])

          firstNonNA <- min(which(!is.na(d_temp$value)))

          # Look for instances of multiple NA values within the dataframe - trail of NA values at the end of the data
          lastNA <- max(which(is.na(d_temp$value)))

          # if the last value is NA, trim the NA values from the end
          if (lastNA == dim(d_temp)[1]) {
            NAs <- data.frame(Location = which(is.na(d_temp$value))) %>%
              # Change
              purrr::map_df(rev) %>%
              dplyr::mutate(difference = c(NA, diff(Location)))

            # First NA value of the tail of NA
            first_na <- NAs$Location[(which(NAs$difference != -1)-1)[1]]

            # trim the NA values from the end
            d_temp_trim <- d_temp[c(-(first_na:lastNA)),]
          } else {
            d_temp_trim <- d_temp
          }

          # Trim the leasing NA values and calculate the rolling 5-day mean
          dftmp[[l]] <- d_temp_trim[c(-(1:firstNonNA-1)),] %>%
            dplyr::select(date_utc, variable, value) %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(value_5 = zoo::rollapply(data = zoo::na.approx(value), # fill in missing data within the stats
                                                   width = 5,
                                                   FUN = mean,
                                                   align = "center",
                                                   fill = NA,
                                                   na.rm = T,
                                                   partial = TRUE)) %>%
            dplyr::mutate(Date = format(date_utc, format = "%d-%b"))
        }
        roll_mean_data <- do.call("rbind.data.frame", dftmp)
        # Merge the five day mean with the statistics
        d_all_stats <- dplyr::full_join(d_all_2, roll_mean_data, by = c("date_utc", "variable", "value"))
      } else {
        # isolate stats and smooth with five day average
        d_all_stats <- d_all_stats_t %>%
          dplyr::filter(variable != 'Current Year')  %>%
          dplyr::filter(!is.na(date_utc)) %>% # get rid of leap year data
          dplyr::filter(!is.na(value)) %>% # get rid of NA data within stats
          dplyr::arrange(variable, date_utc) %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(value_5 = zoo::rollapply(data = zoo::na.approx(value), # fill in missing data within the stats
                                                 width = 5,
                                                 FUN = mean,
                                                 align = "center",
                                                 fill = NA,
                                                 na.rm = T)) %>%
          dplyr::mutate(Date = format(date_utc, format = "%d-%b"))
      }
    }

    # isolate current year data with statistics
    d_all_curr <- getSWE_current(data = df)

    # Subset dates
    min <- subset(d_all_stats, variable == c("min")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(min = value_5)
    Q10 <- subset(d_all_stats, variable == c("Q10")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(Q10 = value_5)
    Q25 <- subset(d_all_stats, variable == c("Q25")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(Q25 = value_5)
    Q50 <- subset(d_all_stats, variable == c("Median")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(Q50 = value_5)
    Q75 <- subset(d_all_stats, variable == c("Q75")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(Q75 = value_5)
    Q90 <- subset(d_all_stats, variable == c("Q90")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(Q90 = value_5)
    max <- subset(d_all_stats, variable == c("max")) %>%
      dplyr::ungroup() %>%
      dplyr::select(date_utc, value_5) %>%
      dplyr::rename(max = value_5)

    # Arrange all the statistics to make the statistics bands in the plot
    bands <- purrr::reduce(list(min, Q10, Q25, Q50, Q75, Q90, max), dplyr::left_join, by = "date_utc") %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(Date = format(date_utc, format = "%d-%b")) %>%
      dplyr::filter(!is.na(max))

    # plot the max and min
    date_min <- as.Date(min(d_all_2$date_utc, na.rm = TRUE))
    date_max <- as.Date(max(d_all_2$date_utc, na.rm = TRUE))

    # Ensure all data rounded
    bands <- round_df(bands, digits = 0) %>%
      dplyr::arrange(date_utc)
    d_historic <- round_df(d_historic, digits = 0)
    d_all_curr <- round_df(d_all_curr, digits = 0)
    d_all_stats <- round_df(d_all_stats, digits = 0)

    # ============================
    # Plot annotations
    # ============================

    # If the ID is 2F01AP, replace with the name
    if (stn_id == "2F01AP") {
      station_name = "Trout Creek West"
    }

    # What was the date of the last available data?
    d_c <- d_all_curr %>%
      dplyr::filter(!is.na(value))
    date_last <- tail(d_c, n = 1)$date_utc

    ## Calculate the days to peak snow normal. Partition if there is >50% of the normals present
    # If there is a normal for the station and there is more that 50% of the data present, calculate statistics
    #if (sum(!is.na(df$normal_Q50)) / length(df$normal_Q50) > 0.5 && all(!is.null(df$normal_Q50))) {
    if (all(!is.null(df$normal_Q50))) {

      norm_q50 <- subset(d_all_stats, variable == "Normal (1981-2010)")
      date_max <- norm_q50$date_utc[norm_q50$value == max(norm_q50$value, na.rm = TRUE)]
      days_till_peak <- as.Date(date_max[1]) - Sys.Date()

      day_peak_1 <- norm_q50$date_utc[norm_q50$value == max(norm_q50$value, na.rm = TRUE)]
      day_peak <- day_peak_1[!is.na(day_peak_1)]

      ## Calculate percent of normal for the last day
      percent_normal_mean <- paste0(as.numeric(df$percent_normal_mean[as.Date(df$date_utc) == date_last]), "%")

      ## Percent of normal peak
      percent_normal_peak <- round(d_all_curr$value[na.omit(d_all_curr$date_utc == date_last)]/max(norm_q50$value_5, na.rm = TRUE) * 100, digits = 0)
      percent_normal_peak <- paste0(percent_normal_peak, "%")

      # Typical Percent of median peak for this date
      typical_percentnorm <- round(norm_q50$value_5[na.omit(norm_q50$date_utc == date_last)] / max(norm_q50$value_5, na.rm = TRUE)*100, digits = 0)
      typical_percentnorm <- paste0(typical_percentnorm, "%")
    } else if (all(is.null(df$normal_Q50)) | all(is.na(df$normal_Q50)) | sum(!is.na(df$normal_Q50)) / length(df$normal_Q50) < 0.5) {

      norm_q50 <- subset(d_all_stats, variable == "Normal (1981-2010)")
      date_max <- "Insufficient data"
      days_till_peak <- "Insufficient data"
      day_peak <- "Insufficient data"
      percent_normal_mean <- "Insufficient data"
      percent_normal_peak <- "Insufficient data"
      typical_percentnorm <- "Insufficient data"
    }

    # Percentile rank
    percentile_today <- df$percentile[df$date_dmy == date_last]

    # Percent of median
    percent_median <- as.numeric(df$percent_Q50[df$date_dmy == date_last])

    # Elevation
    el_site <- data.frame(elevation()) %>%
      dplyr::filter(Station_ID %in% stn_id) %>%
      dplyr::select(Elevation)
    el_site <- as.character(el_site)

    # Year established
    year_est <- min(as.numeric(as.character(d_historic$year)), na.rm = TRUE)

    # Basin
    basin <- gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", as.character(basin_name(stn_id, basin = "All")[1,1]))

    # Owned by
    meta_select <- meta %>%
      dplyr::filter(ID == stn_id)

    owned_by <- as.character(meta_select$OWNER[1])

    # ======================
    # get the increase in SWE from today's date to the max. Based on previous monthly change in SWE statistics
    # ======================

    lastday_data <- d_all_curr$date_utc[max(which(!is.na(d_all_curr$value)))]
    date_peak <- day_peak[1]

    if (class(date_peak) == "Date") { # if there is a calculated peak normal
      deltaSWE <- deltaSWE_datetopeak(df, lastday_data, date_peak)

      #if (dim(deltaSWE)[1] > 1) {
      #  deltaSWE = deltaSWE[which(deltaSWE$date_peak == date_peak), ]
      #}
    }

    # ==========
    # Check whether the peak SWE has passed. If it has, calculate: 1) Percent melted; 2) percent of current year peak SWE versus normal peak
    # ==========
    # Get peak SWE for this year
    max_SWE_currentyear_1 <- d_all_curr %>%
      dplyr::filter(!is.na(value))
    max_SWE_currentyear <- max(max_SWE_currentyear_1$value)

    if (all(is.na(max_SWE_currentyear_1$value))) {
      peakstatus = "No data"
      percentpeak_vs_norm = "No data"
      percentpeak_vs_median = "No data"
    } else {

        if (d_all_curr$value[max(which(!is.na(d_all_curr$value)))] < max_SWE_currentyear-30) {
          peakstatus = "POST"

          # Calculate the percent of the snowpack melted
          percent_SWE_remaining <- round(d_all_curr$value[max(which(!is.na(d_all_curr$value)))]/max_SWE_currentyear, digits = 2)*100

          # Calculate the percent of this year's peak versus peak normal and peak
          percentpeak_vs_norm <- round(max_SWE_currentyear/max(norm_q50$value_5, na.rm = TRUE), digits = 2)*100
          percentpeak_vs_median <- round(max_SWE_currentyear/max(na.omit(Q50$Q50)), digits = 2)*100
        } else {
          peakstatus = "PRE"
          percent_SWE_remaining = 100
          percentpeak_vs_norm <- "Before Peak"
          percentpeak_vs_median <- "Before Peak"
        }
      }

    # ======================
    # plot the SWE across the entire period
    # =======================
    if (dim(d_all_stats)[1] > 0) {

      p <- plotly::plot_ly() %>%
        plotly::add_ribbons(data = bands,
                          x = bands$date_utc,
                          ymax = bands$max,
                          ymin = bands$Q90,
                          fillcolor = list(color = colour_p()$colour_hex[9], opacity = 0),
                          opacity = 0.2,
                          line = list(color = colour_p()$colour_hex[9], opacity = 1, width = 2),
                          name = 'Max - Q90') %>%
        plotly::add_ribbons(data = bands,
                          x = bands$date_utc,
                          ymin = bands$Q75,
                          ymax = bands$Q90,
                          fillcolor = list(color = colour_p()$colour_hex[8], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[8], opacity = 1, width = 2),
                          opacity = 0.2,
                          name = 'Q90 - Q75') %>%
        plotly::add_ribbons(data = bands,
                          x = bands$date_utc,
                          ymin = bands$Q50,
                          ymax = bands$Q75,
                          fillcolor = list(color = colour_p()$colour_hex[7], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[7], opacity = 1, width = 2),
                          opacity = 0.2,
                          name = 'Q75 - Q50') %>%
        plotly::add_ribbons(data = bands,
                          x = bands$date_utc,
                          ymin = bands$Q25,
                          ymax = bands$Q50,
                          fillcolor = list(color = colour_p()$colour_hex[6], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[6], opacity = 1, width = 2),
                          opacity = 0.5,
                          name = 'Q50 - Q25') %>%
        plotly::add_ribbons(data = bands,
                          x = bands$date_utc,
                          ymin = bands$Q10,
                          ymax = bands$Q25,
                          fillcolor = list(color = colour_p()$colour_hex[3], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[3], opacity = 1, width = 2),
                          opacity = 0.2,
                          name = 'Q25 - Q10') %>%
        plotly::add_ribbons(data = bands,
                          x = bands$date_utc,
                          ymin = bands$min,
                          ymax = bands$Q10,
                          fillcolor = list(color = colour_p()$colour_hex[2], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[2], opacity = 1, width = 2),
                          opacity = 0.2,
                          name = 'Min - Q10') %>%
        # stats
        plotly::add_trace(data = subset(d_all_stats, variable == "max"), x = ~date_utc, y = ~as.numeric(value_5),
                        legendgroup = 'Statistics (Max, Q75, Q25, Min)',
                        showlegend = TRUE,
                        type = 'scatter',
                        mode = 'lines',
                        connectgaps = FALSE,
                        color = 'Stats (Max, Q75, Q25, Min)',
                        line = list(color = "rgb(130,130,130)",
                                    width = 2, dash = 'dashdot')) %>%
        plotly::add_trace(data = subset(d_all_stats, variable == "min"), x = ~date_utc, y = ~as.numeric(value_5),
                        legendgroup = 'Statistics (Max, Q75, Q25, Min)',
                        showlegend = F,
                        type = 'scatter',
                        mode = 'lines',
                        connectgaps = FALSE,
                        #color = ~variable,
                        line = list(color = "rgb(130,130,130)", width = 2, dash = 'dashdot')) %>%
        plotly::add_trace(data = subset(d_all_stats, variable == "Q25"), x = ~date_utc, y = ~as.numeric(value_5),
                        legendgroup = 'Statistics (Max, Q75, Q25, Min)',
                        showlegend = F,
                        type = 'scatter', mode = 'lines',
                        connectgaps = FALSE,
                        #color = ~variable,
                        line = list(color = "rgb(153,204,255)",
                                    width = 2, dash = 'dashdot')) %>%
        plotly::add_trace(data = subset(d_all_stats, variable == "Q75"), x = ~date_utc, y = ~as.numeric(value_5),
                        legendgroup = 'Statistics (Max, Q75, Q25, Min)',
                        showlegend = F,
                        type = 'scatter', mode = 'lines',
                        connectgaps = FALSE,
                        #color = ~variable,
                        line = list(color = "rgb(153,204,255)",
                                    width = 2, dash = 'dashdot')) %>%
        plotly::layout(showlegend = T) %>%
        plotly::add_trace(data = subset(d_all_stats, variable == "Normal (1981-2010)"), x = ~date_utc, y = ~as.numeric(value_5),
                        type = 'scatter', mode = 'lines',
                        connectgaps = FALSE,
                        color = ~variable,
                        line = list(color = colour_p()$colour_norm,
                                    width = 2, dash = 'dash'))  %>% # normal
        plotly::add_trace(data = subset(d_all_stats, variable == "Median"), x = ~date_utc, y = ~as.numeric(value_5),
                        type = 'scatter', mode = 'lines',
                        connectgaps = FALSE,
                        color = ~variable,
                        line = list(color = "rgb(130,130,130)",
                                    width = 3, dash = 'dashdot'))  %>% # median (Q50)
        # current year data
        plotly::add_trace(data = d_all_curr, x = ~date_utc, y = ~value, type = 'scatter', mode = 'lines',
                        connectgaps = FALSE,
                        showlegend = TRUE,
                        color = ~variable,
                        line = list(color = "black", width = 4)) %>%
        # past year data
        plotly::add_trace(data = d_historic,
                        x = ~Date_art,
                        y = ~swe_mm,
                        type = 'scatter',
                        mode = 'lines',
                        connectgaps = FALSE,
                        showlegend = TRUE,
                        visible = "legendonly",
                        name = ~wr,
                        #color = ~wr,
                        line = list(color = viridis::viridis(1000), width = 3))
      } else {
        # If there is only this year's data
        p <- plotly::plot_ly() %>%
          plotly::add_trace(data = d_all_curr, x = ~date_utc, y = ~value, type = 'scatter', mode = 'lines',
                          connectgaps = FALSE,
                          showlegend = TRUE,
                          color = ~variable,
                          line = list(color = "black", width = 4)) %>%
          # past year data
          plotly::add_trace(data = d_historic,
                            x = ~Date_art,
                            y = ~swe_mm,
                            type = 'scatter',
                            mode = 'lines',
                            connectgaps = FALSE,
                            showlegend = TRUE,
                            visible = "legendonly",
                            name = ~wr,
                            #color = ~wr,
                            line = list(color = viridis::viridis(1000), width = 3))
      }

      p <- p %>%
        plotly::layout(autosize = T,
                     title = paste0('SWE (mm) for ', station_name, ", ", stn_id),
                     margin = list(l=30, r=30, b=55, t=30, pad=0),
                     xaxis = list(
                       title = paste0(annotation()),
                       titlefont = list(size=8),
                       type = 'date',
                       range = c(min(d_all_stats$date_utc), max(d_all_stats$date_utc)),
                       tickformat = "%d-%B"),
                     yaxis = list(title = 'SWE (mm)')) %>%
        plotly::layout(annotations = list(
          list(x = 0 , y = 1, text = paste0("Elevation (m): ", el_site, " | Owned by: ", owned_by, " | Year established: ", year_est, " | Basin = ", basin), showarrow = F, xref='paper', yref='paper'),
          list(x = 0 , y = 0.98, text = paste0("Current % of normal (1981-2010): ", percent_normal_mean, " | Current % of median: ", percent_median), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.96, text = paste0("% of normal peak: ", percent_normal_peak, " | Typical % of peak accumulation for today: ", typical_percentnorm), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.94, text = paste0("Day of peak: ", day_peak[1], " | Days until normal peak: ", days_till_peak), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.92, text = paste0("Percentile Rank: ", percentile_today, "th"), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.90, text = paste0("*Statistics smoothed by 5-day average| Updated: ", Sys.Date(), " | Stats calculated for: ", lastday_data), showarrow = F, xref='paper', yref='paper')))


    # Add statistics related to the post peak snow conditions
    if (peakstatus == "POST") {
      p <- p %>%
        plotly::layout(annotations = list(
          list(x = 0 , y = 0.88, text = paste0("Percent SWE Remaining from Peak: ", percent_SWE_remaining, "%"), showarrow = F, xref='paper', yref='paper'),
          list(x = 0 , y = 0.86, text = paste0("Percent Year's Peak vs. Normal Peak: ", percentpeak_vs_norm, "% | Percent Year's Peak vs. Median Peak: ",
                                               percentpeak_vs_median, "%"), showarrow = F, xref='paper', yref='paper')))
    }

    # Add the projections if they exist and the time is before the peak
    if (exists("deltaSWE") && peakstatus == "PRE") {
      if (dim(deltaSWE)[1] > 0) {

        p <- p %>%
          plotly::add_trace(data = deltaSWE,
                            x = as.Date(deltaSWE$date_peak),
                            y = as.numeric(deltaSWE$median),
                            legendgroup = 'Projected SWE',
                            type = 'scatter',
                            mode = 'lines',
                            color = "Projected SWE (Max, Q75, Median, Q25, Min)",
                            showlegend = TRUE,
                            visible = "legendonly",
                            line = list(color = "black", width = 2)) %>%
          plotly::add_trace(data = deltaSWE,
                            x = as.Date(deltaSWE$date_peak),
                            y = as.numeric(deltaSWE$MAX),
                            legendgroup = 'Projected SWE',
                            type = 'scatter',
                            mode = 'lines',
                            color = "max",
                            showlegend = F, visible = "legendonly",
                            line = list(color = "red", width = 2)) %>%
          plotly::add_trace(data = deltaSWE,
                            x = as.Date(deltaSWE$date_peak),
                            y = as.numeric(deltaSWE$MIN),
                            legendgroup = 'Projected SWE',
                            type = 'scatter',
                            mode = 'lines',
                            color = "min",
                            showlegend = F,
                            visible = "legendonly",
                            line = list(color = "blue", width = 2)) %>%
          plotly::add_trace(data = deltaSWE,
                            x = as.Date(deltaSWE$date_peak),
                            y = as.numeric(deltaSWE$Q75),
                            legendgroup = 'Projected SWE',
                            type = 'scatter',
                            mode = 'lines',
                            color = "Q75",
                            showlegend = F, visible = "legendonly",
                            line = list(color = "orange", width = 2)) %>%
          plotly::add_trace(data = deltaSWE,
                            x = as.Date(deltaSWE$date_peak),
                            y = as.numeric(deltaSWE$Q25),
                            legendgroup = 'Projected SWE',
                            type = 'scatter',
                            mode = 'lines',
                            color = "Q25",
                            showlegend = F, visible = "legendonly",
                            line = list(color = "light blue", width = 2)) %>%
          plotly::layout(
            showlegend = T, visible = "legendonly"
          )
      }
    }

    p <- plotly::partial_bundle(p) # make the size smaller

    # Save if you have specified that you should save it
    if (save %in% c("True", "true", "T", "TRUE", TRUE)) {
      htmlwidgets::saveWidget(plotly::as_widget(p), paste0(path, "ASWE_", stn_id, ".html"),
                              selfcontained = F, # for making the finished product smaller and faster to save
                              libdir = NULL, # for making the finished product smaller and faster to save
                              title = paste0("SWE ", stn_id))
    }

    # ======================
    # Compile statistics to save in file for the map
    # ======================
    SWE_today <- d_all_curr$value[d_all_curr$date_utc == as.Date(date_last)][1]

    if (length(date_last) == 0) {
      date_last = NA
    }

    #if (all(is.na(SWE_today))) { # If there is no current data to save, save an empty dataframe so it still shows up on the map
    if (!(date_last %in% c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2))) {
      print("No current stats to save")

      empty_df <- data.frame("id" = NA, "station_name" = NA, "el_site" = NA,
                             "owned_by" = NA, "year_est" = NA, "basin" = NA,
                             "SWE_today" = "No Data",
                             "percent_median" = NA,
                             "percentile_today" = NA,
                             "percent_normal" = NA, "days_till_peak" = days_till_peak, "day_peak" = day_peak[1],
                             "date_stats" = date_last)
      empty_df$id <- stn_id
      empty_df$station_name <- ifelse(length(station_name) > 0, station_name, NA)
      empty_df$el_site <- el_site
      empty_df$owned_by <- owned_by
      empty_df$year_est <- year_est
      empty_df$basin <- basin

      stats_out <- empty_df
    } else {
      if (length(percent_median) == 0){percent_median = NA}
      if (length(percentile_today) == 0){percentile_today = NA}

      stats_out_i <- data.frame("id" = stn_id, "station_name" = station_name,
                                "el_site" = el_site, "owned_by" = owned_by, "year_est" = year_est, "basin" = basin,
                                "SWE_today" = SWE_today, "percent_median" = percent_median,
                                "percentile_today" = percentile_today, "percent_normal_mean" = percent_normal_mean,
                                "days_till_peak" = as.numeric(days_till_peak), "day_peak" = day_peak[1],
                                "date_stats" = date_last)

      row.names(stats_out_i) <- c()

      # Add in the density for today
      snow_depth <- bcsnowdata::get_aswe_databc(station_id = stn_id,
                                                get_year = bcsnowdata::wtr_yr(Sys.Date()),
                                                parameter = "snow_depth",
                                                timestep = "daily")
      if ("value" %in% colnames(snow_depth))  {
        snow_depth_cm <- snow_depth %>%
          dplyr::rename(SnowDepth_cm = value) %>%
          dplyr::select(date_utc, SnowDepth_cm, id) %>%
          dplyr::filter(date_utc == Sys.Date()) %>%# choose today's data
          dplyr::distinct(SnowDepth_cm, .keep_all = TRUE)

        # Calculate density
        snow_density <- dplyr::full_join(stats_out_i, snow_depth_cm) %>%
          dplyr::mutate(swe_mm = as.numeric(as.character(SWE_today))) %>%
          dplyr::mutate(SWE_cm = swe_mm / 10) %>%
          dplyr::mutate(density_cmcm = round(SWE_cm / SnowDepth_cm, digits = 2)) %>%
          dplyr::mutate(density_cmcm = replace(density_cmcm, density_cmcm >1.0, ">1.0"))
      } else {
        snow_density <- stats_out_i %>%
          dplyr::mutate(SnowDepth_cm = NA) %>%
          dplyr::mutate(swe_mm = as.numeric(as.character(SWE_today))) %>%
          dplyr::mutate(SWE_cm = swe_mm / 10) %>%
          dplyr::mutate(density_cmcm = NA)
      }

      stats_out <- snow_density %>%
        # Add in the post-peak SWE statistics
        dplyr::mutate(percentSWE_remaining = percent_SWE_remaining) %>%
        dplyr::mutate(percentpeak_vsnorm = percentpeak_vs_norm) %>%
        dplyr::mutate(percentpeak_vsmedian = percentpeak_vs_median)

      # Save statistics csv file
      if (save %in% c("True", "true", "T", "TRUE", TRUE)) {

        # Check that the file folder exists
        if (file.exists(paste0(path, "Stats"))) {
          stats_path <- paste0(path, "Stats/")
          write.csv(stats_out, file = paste0(stats_path, stn_id, ".csv"), row.names = FALSE)
        } else {
          stats_path <- paste0(path, "/Stats/")
          dir.create(paste0(path, "/Stats/"), showWarnings = TRUE, recursive = FALSE, mode = "0777")
          write.csv(stats_out, file = paste0(stats_path, stn_id, ".csv"), row.names = FALSE)
        }
      }
    }

    out <- (list("SWEplot" = p, "stats" = stats_out))
  } else {
    print("No data for site")
    out <- NULL
  }
}

# ================================================
#' Interactive plots - ASWE sites (plot delta SWE and climate plots)
#' @param path path that you want to save plots in
#' @param id station ID that you want to plot SWE for
#' @param save whether to save the plot and stats. Defaults to 'No', Options are also TRUE
#' @importFrom magrittr %>%
#' @importFrom data.table %like%
#' @importFrom dplyr bind_rows
#' @export
#' @keywords plot ASWE SWE
#' @examples \dontrun{}
plot_interactive_aswe <- function (path, id, save = "No") {

  # Get statistics data for the site you are plotting
  data_plot_1 <- get_swe(id)

  # Run function over all sites that you have
  plots <- lapply(unique(id),
                  plot_function,
                  data_plot_1,
                  save,
                  path)

}

# ================
#' Function for plotting climate data from ASWE sites
#' Plot the temp, precip and delta SWE as a separate plot
#' @param path path that you want to save plots in
#' @param id station ID that you want to plot SWE for
#' @param save whether to save the plot and stats. Defaults to 'No'
#' @importFrom magrittr %>%
#' @export
#' @keywords plot climate aswe
#' @examples \dontrun{}
plot_climate_aswe <- function(path, id, save = "No") {

  # -------------------------
  # Get data for all stations
  # -------------------------

  # get the temp for the station
  temp_raw <- bcsnowdata::get_aswe_databc(station_id = id,
                         get_year = bcsnowdata::wtr_yr(Sys.Date()),
                         parameter = "temperature",
                         timestep = "daily")

  temp <- temp_raw %>%
   dplyr::mutate(Date = as.Date(date_utc)) %>%
   dplyr::group_by(id, Date) %>%
   dplyr::mutate(Daily_max = max(value, na.rm = TRUE)) %>%
   dplyr::mutate(Daily_min = min(value, na.rm = TRUE)) %>%
   dplyr::select(Date, Daily_max, Daily_min) %>%
   dplyr::distinct(Date, .keep_all = TRUE) %>%
   dplyr::ungroup() %>%
   dplyr::mutate(Date = as.Date(Date)) # ensure the date is in the right format

  # -------------------------
  # Get the precip data and calculate daily change
  # -------------------------
  precip <- bcsnowdata::get_aswe_databc(station_id = id,
                                        get_year = bcsnowdata::wtr_yr(Sys.Date()),
                                        parameter = "precipitation",
                                        timestep = "daily") %>%
    dplyr::distinct(date_utc, .keep_all = TRUE) %>%
    dplyr::group_by(id)

  # -------------------------
  ## delta SWE
  # -------------------------
  # Get statistics data for the site you are plotting
  swe <- get_swe(id)

  # Plot all three together
  plot_climate <- function(station, temp, precip, swe, save) {

      station_name <- bcsnowdata::snow_auto_location()$LOCATION_NAME[bcsnowdata::snow_auto_location()$LOCATION_ID %in% as.character(station)]

      # temperature
      temp_p <- temp %>%
        dplyr::filter(id %in% station) %>%
        plotly::plot_ly() %>%
        plotly::add_trace(x = ~Date, y = ~Daily_max, name= "Daily Max Temp", type = 'scatter', mode = 'lines', connectgaps = FALSE) %>%
        plotly::add_trace(x = ~Date, y = ~Daily_min, type = 'scatter', type = 'scatter', mode = 'lines',
            line = list(color = "grey"), name= "Daily Min Temp", connectgaps = FALSE) %>%
        plotly::layout(title = paste0('Daily Temp (degree C) for ', station_name, ", ", station),
            xaxis = list(
            title = 'Date',
            type = 'date',
            tickformat = "%d-%B"),
            yaxis = list(title = 'Daily Temp (degree C)'))

      # Precipitation
      precip_id <- precip %>%
        dplyr::filter(id %in% station)

      # calculate the difference in precipitation between days
      precip_diff <- diff(precip_id$value)
      if (length(precip_diff) > 0) {
        delta_precip <-  data.frame(R.utils::insert(precip_diff, 1, NA)) %>% # insert leading NA
          dplyr::mutate(date_utc = precip_id$date_utc) %>%
          dplyr::rename(deltaprecip_daily_mm = "R.utils..insert.precip_diff..1..NA.") %>%
          dplyr::mutate(plus_neg = ifelse(deltaprecip_daily_mm >= 0, "green", "red"))
      } else {
        delta_precip <- data.frame(date_utc = NA, deltaprecip_daily_mm = NA, plus_neg = NA)
      }

      if (length(delta_precip) > 0) {
        precip_p <- delta_precip %>%
          plotly::plot_ly() %>%
          # add_trace(x = ~date_utc, y = ~value, name= "Accumulated Precip", type = 'scatter', mode = 'markers') %>%
          plotly::add_bars(data = subset(delta_precip, plus_neg == 'green'), x = ~date_utc, y = ~deltaprecip_daily_mm, type = 'bar', marker = list(color = "green"),
            name = "Change in Daily Accumulated Precip") %>%
          #add_bars(data = subset(delta_precip, plus_neg == 'red'), x = ~date_utc, y = ~deltaprecip_daily_mm, type = 'bar', marker = list(color = "red"),
          #          name = 'Loss') %>%
          plotly::layout(title = paste0('Increase in Daily Precipitation for ', station_name, ", ", station),
            xaxis = list(
              title = 'Date',
              type = 'date',
              tickformat = "%d-%B"),
            yaxis = list(title = 'Increase in Daily Precipitation (mm) <br> <i>*Only increases in precip shown</i>'))
        } else {
          precip_p <- plotly::plot_ly() # Assign an empty plot
        }

      # ------------------------
      # Plot barchart of the delta SWE
      swe_id <- swe %>%
        dplyr::filter(id %in% station)

      # IF there is data, plot it. If not, assign the data as NA
      if (dim(swe_id)[1] > 1) {

        # isolate current year data with statistics
        d_all_curr <- getSWE_current(data = swe_id)

        # Calculate the delta SWE from the current year - d_all_curr
        delta_SWE <-  diff(d_all_curr$value)
        delta_SWE_full <-  data.frame(R.utils::insert(delta_SWE, 1, NA)) %>% # insert leading NA
          dplyr::mutate(date_utc = d_all_curr$date_utc) %>%
          dplyr::rename(deltaSWE_daily_mm = "R.utils..insert.delta_SWE..1..NA.") %>%
          dplyr::mutate(plus_neg = ifelse(deltaSWE_daily_mm >= 0, "blue", "red"))

        delta_all <- dplyr::full_join(d_all_curr, delta_SWE_full, by = "date_utc")
      }

      if (exists("delta_all")) {
        delta_p <- plotly::plot_ly() %>%
          plotly::add_bars(data = subset(delta_all, plus_neg == 'blue'), x = ~date_utc, y = ~deltaSWE_daily_mm, type = 'bar', marker = list(color = "blue"),
              name = "Accumulation") %>%
          plotly::add_bars(data = subset(delta_all, plus_neg == 'red'), x = ~date_utc, y = ~deltaSWE_daily_mm, type = 'bar', marker = list(color = "red"),
              name = 'Loss') %>%
          plotly::layout(title = paste0('Change in Daily SWE (mm) for ', station_name, ", ", station),
              xaxis = list(
                title = 'Date',
                type = 'date',
                tickformat = "%d-%B"),
            yaxis = list(title = 'Change in Daily SWE (mm)'))
      } else {
        delta_p <- plotly::plot_ly() # Assign an empty plot
      }

      ## Save the precip, temp and delta SWE as one plot
      all_p <-  plotly::subplot(temp_p, precip_p, delta_p, nrows = 3, margin = 0.04, heights = c(0.3, 0.3, 0.4), shareX = TRUE, titleY = TRUE)

      if (save %in% c("True", "true", "T", "TRUE", TRUE)) {
        htmlwidgets::saveWidget(plotly::as_widget(all_p),
                           paste0(path, "delta_", station, ".html"),
                           selfcontained = F,
                           libdir = NULL,
                           title = paste0("SWE_Change ", station))
      }

      return(list("climate_plot" = all_p))
    }

    # Run function over all stations
    plots <- lapply(id,
                    plot_climate,
                    temp, precip, swe, save)
}

# ================================================
#' Plotting manual sites
#' @param path path that you want to save plots in
#' @param id station ID that you want to plot SWE for (manual)
#' @param save whether to save the plot and stats. Defaults to 'No'. Can also be TRUE
#' @importFrom magrittr %>%
#' @importFrom data.table %like%
#' @export
#' @keywords plot manual SWE
#' @examples \dontrun{}
plot_interactive_manual <- function(id, path, save = "No") {

  # Get the data with statistics for the manual site you are interested in
  data_plot_1 <- get_snow_stats(station_id = id,
                                survey_period = "All",
                                get_year = "All",
                                normal_min = 1991,
                                normal_max = 2020,
                                force = FALSE)
  # Plot with function
  p <- do.call(rbind, lapply(id, plot_manual, data_plot_1, save, path))

  p
}

plot_manual <- function(station, data_plot_1, save, path) {
  df_p <- data_plot_1 %>%
    dplyr::filter(id %in% station)

  #Isolate only the name
  station_name <- unique(df_p$snow_course_name)[!is.na(unique(df_p$snow_course_name))]

  water_year <- bcsnowdata::wtr_yr(Sys.Date())
  df_p <- round_df(x = df_p, digits = 0) # round all numerics within data to the nearest whole number

  if (dim(df_p)[1] <= 1) { # skip the loop if there is no data for the site
    print(paste0("No data returned for site ", station_name))
    p <- NULL
  } else {

    # Get current year and stats
    data_plot_current <- df_p %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::filter(wr == bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::ungroup() %>%
      dplyr::select("date_utc", "swe_mm") %>%
      dplyr::mutate(date_utc = as.Date(date_utc))

    # =================
    # Historic data
    # =================
    data_plot_historic <- df_p %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::ungroup() %>%
      dplyr::select("date_utc", "swe_mm", wr, "survey_period") %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::mutate(year = lubridate::year(date_utc)) %>%
      dplyr::mutate(month = lubridate::month(date_utc)) %>%
      dplyr::group_by(wr) %>%
      dplyr::mutate(year_art = ifelse(month >=10, water_year-1, water_year)) %>%
      dplyr::mutate(Date_art = as.Date(paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc), "-", year_art),
                                       format = "%d-%m-%Y")) %>%
      dplyr::select(-year_art, -month) %>%
      dplyr::ungroup()  %>%
      dplyr::mutate(year = as.factor(year)) %>%
      dplyr::filter(wr < lubridate::year(Sys.Date())) %>%
      dplyr::mutate(wr = as.factor(wr)) %>%
      #dplyr::mutate(Date = format(date_utc, format = "%d-%b")) %>%
      dplyr::arrange(desc(year)) %>% # attempt to re-arrrange plots so that current years are first
      dplyr::mutate(year_plot = factor(wr, levels = rev(levels(wr)))) %>%
      dplyr::arrange(desc(wr), desc(date_utc))

    # =================
    # Statistics data
    # =================
    data_statistics <- df_p %>%
      dplyr::select(id, survey_period, normal_mm, min, swe_mean, Q5, Q10, Q25, Q50, Q75, Q90, max, data_range, numberofyears) %>%
      dplyr::distinct_all() %>%
      dplyr::mutate(Date = paste0(survey_period, "-", bcsnowdata::wtr_yr(Sys.Date()))) %>%
      dplyr::mutate(Date = as.Date(Date, format = "%d-%b-%Y")) %>%
      dplyr::ungroup()

    data_stats_melt <- data_statistics %>%
      dplyr::select(-data_range, -numberofyears, -normal_mm, -survey_period, -swe_mean, -Q5) %>%
      reshape2::melt(id = c("Date", "id")) %>%
      dplyr::mutate(point_colour = dplyr::case_when(
        variable == "min" ~ colour_p()$colour_hex[2],
        variable == "Q10" ~ colour_p()$colour_hex[2],
        variable == "Q25" ~ colour_p()$colour_hex[3],
        variable == "Q50" ~ colour_p()$colour_hex[6],
        variable == "Q75" ~ colour_p()$colour_hex[7],
        variable == "Q90" ~ colour_p()$colour_hex[8],
        variable == "max" ~ colour_p()$colour_hex[9])) %>%
      dplyr::arrange(variable, Date)

    # =================
    # Plot Annotations
    # =================
    elevation <- unique(df_p$elev_metres)[1]

    # Not owned by anybody. Maintained by?
    #owned_by <- subset(meta, ID == id)$OWNER

    year_est <- lubridate::year(unique(min(df_p$date_utc)))
    # basin # Basin
    basin <- basin_name(station, basin = "All")
    basin <- gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", as.character(basin[1,1]))
    percent_normal <- data.table::last(df_p$percent_normal_mean)
    percent_median <- data.table::last(df_p$percent_normal_median)

    percentile_last <- df_p$percentile[dim(df_p)[1]]
    date_percentile <- df_p$date_utc[dim(df_p)[1]]

    # =================
    # Do plot
    # =================
    p <- plotly::plot_ly() %>%
      plotly::add_ribbons(data = data_statistics,
                          x = data_statistics$Date,
                          ymax = data_statistics$max,
                          ymin = data_statistics$Q90,
                          connectgaps = TRUE,
                          fillcolor = list(color = colour_p()$colour_hex[9], opacity = 0),
                          opacity = 0.5,
                          line = list(color = colour_p()$colour_hex[9], opacity = 1, width = 2),
                          name = 'Max - Q90') %>%
      plotly::add_ribbons(data = data_statistics,
                          x = data_statistics$Date,
                          ymin = data_statistics$Q75,
                          ymax = data_statistics$Q90,
                          connectgaps = TRUE,
                          fillcolor = list(color = colour_p()$colour_hex[8], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[8], opacity = 1, width = 2),
                          opacity = 0.5,
                          name = 'Q90 - Q75') %>%
      plotly::add_ribbons(data = data_statistics,
                          x = data_statistics$Date,
                          ymin = data_statistics$Q50,
                          ymax = data_statistics$Q75,
                          connectgaps = TRUE,
                          fillcolor = list(color = colour_p()$colour_hex[7], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[7], opacity = 1, width = 2),
                          opacity = 0.5,
                          name = 'Q75 - Q50') %>%
      plotly::add_ribbons(data = data_statistics,
                          x = data_statistics$Date,
                          ymin = data_statistics$Q25,
                          ymax = data_statistics$Q50,
                          connectgaps = TRUE,
                          fillcolor = list(color = colour_p()$colour_hex[6], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[6], opacity = 1, width = 2),
                          opacity = 0.6,
                          name = 'Q50 - Q25') %>%
      plotly::add_ribbons(data = data_statistics,
                          x = data_statistics$Date,
                          ymin = data_statistics$Q10,
                          ymax = data_statistics$Q25,
                          connectgaps = TRUE,
                          fillcolor = list(color = colour_p()$colour_hex[3], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[3], opacity = 1, width = 2),
                          opacity = 0.5,
                          name = 'Q25 - Q10') %>%
      plotly::add_ribbons(data = data_statistics,
                          x = data_statistics$Date,
                          ymin = data_statistics$min,
                          ymax = data_statistics$Q10,
                          connectgaps = TRUE,
                          fillcolor = list(color = colour_p()$colour_hex[2], opacity = 0.9),
                          line = list(color = colour_p()$colour_hex[2], opacity = 1, width = 2),
                          opacity = 0.5,
                          name = 'Min - Q10') %>%
      # stats
      plotly::add_trace(data = subset(data_stats_melt, variable == "min"),
                        x = ~Date,
                        y = ~value,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = FALSE,
                        #color = ~variable,
                        #colors = ~point_colour,
                        name = "Minimum",
                        marker = list(color = colour_p()$colour_hex[2],
                                      size = 10,
                                      opacity = 0.8),
                        line = list(color = colour_p()$colour_hex[2],
                                    width = 2,
                                    dash = 'dashdot',
                                    opacity = 0.9)) %>%
      plotly::add_trace(data = subset(data_stats_melt, variable == "max"),
                        x = ~Date,
                        y = ~value,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = FALSE,
                        #color = ~variable,
                        #colors = ~point_colour,
                        name = "Maximum",
                        marker = list(color = colour_p()$colour_hex[9],
                                      size = 10,
                                      opacity = 0.8),
                        line = list(color = colour_p()$colour_hex[9],
                                    width = 2,
                                    dash = 'dashdot',
                                    opacity = 0.9)) %>%
      plotly::add_trace(data = subset(data_stats_melt, variable == "Q25"),
                        x = ~Date,
                        y = ~value,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = FALSE,
                        #color = ~variable,
                        #colors = ~point_colour,
                        name = "25th Percentile",
                        marker = list(color = colour_p()$colour_hex[6],
                                      size = 10,
                                      opacity = 0.8),
                        line = list(color = colour_p()$colour_hex[6],
                                    width = 2,
                                    dash = 'dashdot',
                                    opacity = 0.9)) %>%
      plotly::add_trace(data = subset(data_stats_melt, variable == "Q75"),
                        x = ~Date,
                        y = ~value,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = FALSE,
                        #color = ~variable,
                        #colors = ~point_colour,
                        name = "75th Percentile",
                        marker = list(color = colour_p()$colour_hex[7],
                                      size = 10,
                                      opacity = 0.8),
                        line = list(color = colour_p()$colour_hex[7],
                                    width = 2,
                                    dash = 'dashdot',
                                    opacity = 0.9)) %>%
      plotly::add_trace(data = subset(data_stats_melt, variable == "Q50"),
                        x = ~Date, y = ~value,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = FALSE,
                        name = "Median",
                        marker = list(color = "rgb(130,130,130)",
                                      size = 10,
                                      opacity = 0.8),
                        line = list(color = "rgb(130,130,130)",
                                    width = 2,
                                    dash = 'dashdot',
                                    opacity = 0.9)) %>%
      # current year data
      plotly::add_trace(data = data_plot_current,
                        x = ~date_utc,
                        y = ~swe_mm,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = TRUE,
                        name = "Current year",
                        #color = ~variable,
                        marker = list(color = "black",
                                      size = 10,
                                      width = 4),
                        line = list(color = "black",
                                    width = 3,
                                    dash = 'dashdot')) %>%
      # past year data
      plotly::add_trace(data = data_plot_historic,
                        x = ~Date_art,
                        y = ~swe_mm,
                        type = 'scatter',
                        mode = 'lines+markers',
                        connectgaps = TRUE,
                        showlegend = TRUE, visible = "legendonly",
                        color = ~year_plot,
                        marker = list(size = 10,
                                      width = 4),
                        line = list(width = 3,
                                    dash = 'dashdot')) %>%
      plotly::layout(title = paste0('Manual SWE (mm) for ', station_name, ", ", station),
                     margin = list(l=30, r=30, b=80, t=30, pad=0),
                     xaxis = list(
                       title = paste0(annotation()),
                       titlefont = list(size=8),
                       automargin = TRUE,
                       type = 'date',
                       #range = c(min(df.tmp.1$DATE), max(df.tmp.1$DATE)),
                       tickformat = "%d-%B"),
                     yaxis = list(title = 'SWE (mm)')) %>%
      plotly::layout(annotations = list(
        list(x = 0 , y = 1, text = paste0("Elevation (m): ", elevation, " | Owned by: ",  " | Year established: ", year_est, " | Basin = ", basin), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.98, text = paste0("Current % of normal (1981-2010): ", percent_normal, " | Current % of median: ", percent_median), showarrow = F, xref='paper', yref='paper'),
        #list(x = 0 , y = 0.96, text = paste0("% of normal peak: ", percent_normal_peak, " | Typical % of peak accumulation for today: ", typical_percentnorm), showarrow = F, xref='paper', yref='paper'),
        #list(x = 0 , y = 0.94, text = paste0("Day of peak: ", day_peak[1], " | Days until normal peak: ", days_till_peak), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.96, text = paste0("Percentile Rank: ", percentile_last, "th | Survey date of percentile: ", date_percentile), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.94, text = paste0("*Statistics smoothed by 5-day average | Updated: ", Sys.Date()), showarrow = F, xref='paper', yref='paper')))

    p <- plotly::partial_bundle(p) # make the size smaller

    # Save the plot if you have specified that it should be saved.
    if (save %in% c("True", "true", "T", "TRUE", TRUE)) {
      htmlwidgets::saveWidget(plotly::as_widget(p),
                              paste0(path, "manual_plots/", station, ".html"),
                              selfcontained = F,
                              libdir = NULL,
                              title = paste0("SWE ", station, " ", station_name))
    }
  }
}

