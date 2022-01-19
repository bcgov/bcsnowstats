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
#=======================

# Utilities for the bcsnow_analysis function
# =======================

# =======================
#' Functions for returning the rank of a value - by min
#' January 2021, Ashlee Jollymore
#' @param x data
#' @param y column
#' @keywords internal
#' @examples \dontrun{}

rank_min_function <- function(x, y) {
  current_rank_min <- rank(as.numeric(c(paste0(unlist(x)), y)))[length(c(paste0(x), y))]
}

# ===========================
#' Function for returning the rank of a value - by max
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param x data
#' @param y column
#' @keywords internal
#' @examples \dontrun{}

rank_max_function <- function(x, y) {
  current_rank_max <- rank(-as.numeric(c(paste0(unlist(x)), y)))[length(c(paste0(x), y))]
}

# ===========================
#' Datafilling function
#' Function for filling in NA with 0 when there is no snow at the ASWE pillow and Aquarius has cut out the negative noise
#' Part of calculating normals workflow for ASWE data.
#' January 2021, Ashlee Jollymore
#' @param data data from the station you are trying to fill in data for
#' @keywords internal
#' @examples \dontrun{}

fillNA0 <- function(data, ...) {

  # Group by water year
  # Find the NA values.
  # If the last value before the NA value was 0, carry the 0 forward through NAs

  data_na <- data %>%
    dplyr::group_by(wr)

  #where are the NA values?
  NAvalues <- which(is.na(data_na$mean_day), arr.ind = TRUE)

  # if there are no NA values, skip everything
  if (length(NAvalues) > 0) {

    # Create data frame with NA values
    na_sub <- data_na[NAvalues, ]
    # Create a column with the difference in days - identify stretches of NA values
    na_sub$Seq <- c(100, diff(na_sub$date_dmy))

    # Merge with the data
    data_naseq <- dplyr::full_join(data_na, dplyr::select(na_sub, Seq, wr, date_dmy), by = c("date_dmy", "wr"))

    # Identify the first instance of NA values in the dataset - when the Seq column is >1
    # What is the last non NA value?
    first_Na <- which(data_naseq$Seq > 1, arr.ind = TRUE)
    last_NA <- data_naseq[first_Na - 1, ] %>%
      dplyr::filter(mean_day <= 10) %>% # filter only those instances where the last non-NA value is 10 mm or less
      dplyr::mutate(flag = 1)

    # Bind to NA values
    df_na <- dplyr::full_join(last_NA, na_sub, by = c("date_utc", "station_id", "value", "code", "variable", "station_name", "wr", "m_d", "date_dmy", "mean_day", "Seq")) %>%
      dplyr::arrange(date_utc)

    df_na$Seq_2 <- c(100, diff(df_na$date_dmy))

    # Fill in the flag column - times when the date is right after the first 0 NA value
    first_0 <- which(df_na$flag == 1, arr.ind = TRUE) + 1

    # If there is no instances to fill in, return the same data as input

    if (length(first_0) > 0) {

      flag2 <- first_0[which(df_na[first_0, ]$Seq_2 == 1)]

      if (length(flag2) > 0) {
        df_na[flag2, ]$flag <- 2
        df_na[which(df_na$flag == 2, arr.ind = TRUE), ]$Seq_2 = 1
      }

      if (length(which(df_na$flag == 1, arr.ind = TRUE)) > 0) {
        # rename the seq 2 when the flag is 1 or two to filter dataframe
        df_na[which(df_na$flag == 1, arr.ind = TRUE), ]$Seq_2 = 1
      }

      df_na_2 <- df_na %>%
        dplyr::filter(Seq_2 < 2)

      df_na_2 <- dplyr::mutate(df_na_2, Seq_2 = c(NA, diff(date_dmy)))

      if (length(which(df_na_2$flag == 2)) > 0) {
        df_na_2[which(df_na_2$flag == 2, arr.ind = TRUE), ]$Seq_2 <- 1
      }

      if (length(which(df_na_2$flag == 1, arr.ind = TRUE)) > 0){
        df_na_2[which(df_na_2$flag == 1, arr.ind = TRUE), ]$Seq_2 <- 1
      }

      # Take out beginning
      df_na_3 <- df_na_2[(which(df_na_2$flag == 1, arr.ind = TRUE)[1]):dim(df_na_2)[1], ]

      # Show where the first non-na value starts in the dataframe
      starts <- which(df_na_3$flag == 1, arr.ind = TRUE)

      # Get the difference between the date and the last non-na value
      df <- list()
      for (i in 1:length(starts)) {

        firstNAseq <- starts[i]

        if (i != length(starts)) {
          temp <- df_na_3[starts[i]:(starts[i + 1] - 1), ]
        } else {
          temp <- df_na_3[starts[i]:dim(df_na_3)[1], ]
        }

        date_i <- df_na_3[starts[i], ]$date_dmy

        temp$Seq_3 <- temp$date_dmy - date_i

        # If the day after the first non-na value immediately follows it, fill in 0
        if (dim(temp)[1] == 1) {

          # If there is only one instance, the data out is the same as the data in
          temp_out <- temp
        } else if (temp$Seq_3[2] == 1) {

          # Filter so that you are taking only 30 days after the last non-NA value - not extending a 0 past 20 days
          temp <- temp %>%
            dplyr::filter(Seq_3 <= 30)# %>%
           # dplyr::mutate(flag = 1:n())

          # indicate that the 0 is filled in via the code
          temp[is.na(temp$mean_day), ]$code <- "0Filledin"

          # fill in the NA values
          temp[is.na(temp$mean_day), ]$mean_day <- 0

          temp_out <- temp
          #dplyr::select(-Seq, -flag, -Seq_2, -Seq_3)

        } else if (temp$Seq_3[2] > 1) {
          temp_out <- NULL
        }

        df[[i]] <- temp_out
      }

      # Unwrap the list you just made
      unwrap <- do.call(dplyr::bind_rows, df) %>%
        dplyr::select(-Seq, -flag, -Seq_2, -Seq_3)

      # Replace data within the initial dataset with dataset you just created
      # Remove the entries that exist within the new dataframe in the initial dataset

      data_tomerge <- data[!(data$date_dmy %in% unwrap$date_dmy), ]

      data_final <- dplyr::full_join(data_tomerge, unwrap, by = c("date_utc", "station_id", "value", "code", "variable", "station_name", "wr", "m_d", "date_dmy", "mean_day")) %>%
        dplyr::arrange(date_utc)

    } else if (length(first_0) == 0) {
      # If there is no data to fill in, simply return the data that was input into the function
      data_final <- data
    }
  } else if (length(NAvalues) == 0) {
    data_final <- data
  }
  return(data_final)
}

# ===========================
#' Function for rounding all digits within a data frame to specific number of digits
#' @param x data frame
#' @param digits number of digits you want to round data to
#' @keywords internal
#' @examples \dontrun{}
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, is.numeric)
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


# =====================
#' Function for returning elevation by station
#' @export
#' @keywords snow elevations
#' @examples \dontrun{}
elevation <- function() {
  manual_data <- bcsnowdata::snow_manual_location() %>%
    #dplyr::filter(!is.na(SWE_mm)) %>%
    dplyr::arrange(LOCATION_ID) %>%
    dplyr::select(LOCATION_NAME, LOCATION_ID, ELEVATION) %>%
    dplyr::rename(Elevation = ELEVATION) %>%
    dplyr::rename(Name = LOCATION_NAME) %>%
    dplyr::rename(Station_ID = LOCATION_ID) %>%
    dplyr::distinct(Station_ID, .keep_all = TRUE)

  auto <- bcsnowdata::snow_auto_location() %>%
    dplyr::rename(Station_ID = LOCATION_ID) %>%
    dplyr::select(Station_ID, ELEVATION, LOCATION_NAME, geometry) %>%
    dplyr::rename(Elevation = ELEVATION, Name = LOCATION_NAME)

  stations_el <- bind_rows(auto, manual_data)
}

# =====================
#' Function for assigning a basin name to a station ID
#' @param id id of station you are assigning to a basin
#' @param basin name of basin
#' @param exceptions stations to skip
#' @importFrom data.table %like%
#' @export
#' @keywords snow site basin name
#' @examples \dontrun{}
basin_name <- function(id = "All", basin = "All", exceptions = NULL) {

  # get all of the sites within the archive
  all_sites <- unique(c(bcsnowdata::snow_auto_location()$LOCATION_ID, bcsnowdata::snow_manual_location()$LOCATION_ID))

  # Apply exceptions - remove sites that should be removed
  all_sites <-  all_sites[!all_sites %in% exceptions]

  # associate basins by the ID number
  basins_all <- snow_basins()
  sites_first <- data.frame(Basin = basins_all, Station_ID_used = 2)
  sites_first[sites_first$Basin == "UpperFraserWest",][2] <- paste(c("1A04", "1A07", "1A12", "1A16", "1A23", "1A24", "1A12P"), collapse = ";")
  sites_first[sites_first$Basin == "UpperFraserEast",][2] <- paste(c("1A01P", "1A02P", "1A03P", "1A05P", "1A14P", "1A15P", "1A17P", "1A19P",
                                                                     "1A05","1A06", "1A06A","1A08", "1A09", "1A10",
                                                                     "1A11", "1A13", "1A15", "1A18", "1A20", "1A21", "1A22"), collapse = ";")
  sites_first[sites_first$Basin == "Nechako",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "B"& substring(all_sites, 1, 1) == "1"), collapse = ";")
  sites_first[sites_first$Basin == "MiddleFraser",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "C" & substring(all_sites, 1, 1) == "1"), collapse = ";")
  sites_first[sites_first$Basin == "LowerFraser",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "D" & substring(all_sites, 1, 1) == "1"), collapse = ";")
  sites_first[sites_first$Basin == "NorthThompson",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "E" & substring(all_sites, 1, 1) == "1"), collapse = ";")
  sites_first[sites_first$Basin == "SouthThompson",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "F" & substring(all_sites, 1, 1) == "1"), collapse = ";")
  sites_first[sites_first$Basin == "UpperColumbia",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "A" & substring(all_sites, 1, 1) == "2"), collapse = ";")
  sites_first[sites_first$Basin == "WestKootenay",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) %in% c("D","B") & substring(all_sites, 1, 1) == "2"), collapse = ";")
  sites_first[sites_first$Basin == "EastKootenay",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "C" & substring(all_sites, 1, 1) == "2"), collapse = ";")
  sites_first[sites_first$Basin == "Okanagan",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "F" & substring(all_sites, 1, 1) == "2"), collapse = ";")
  sites_first[sites_first$Basin == "Boundary",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "E" & substring(all_sites, 1, 1) == "2"), collapse = ";")
  sites_first[sites_first$Basin == "Similkameen",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "G" & substring(all_sites, 1, 1) == "2"), collapse = ";")
  sites_first[sites_first$Basin == "SouthCoast",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "A" & substring(all_sites, 1, 1) == "3"), collapse = ";")
  sites_first[sites_first$Basin == "VancouverIsland",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "B" & substring(all_sites, 1, 1) == "3"), collapse = ";")
  sites_first[sites_first$Basin == "CentralCoast",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "C" & substring(all_sites, 1, 1) == "3"), collapse = ";")
  sites_first[sites_first$Basin == "Skagit",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "D" & substring(all_sites, 1, 1) == "3"), collapse = ";")
  sites_first[sites_first$Basin == "Peace",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "A" & substring(all_sites, 1, 1) == "4"), collapse = ";")
  sites_first[sites_first$Basin == "SkeenaNass",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "B" & substring(all_sites, 1, 1) == "4"), collapse = ";")
  sites_first[sites_first$Basin == "Liard",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "C" & substring(all_sites, 1, 1) == "4"), collapse = ";")
  sites_first[sites_first$Basin == "Stikine",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "D" & substring(all_sites, 1, 1) == "4"), collapse = ";")
  sites_first[sites_first$Basin == "Northwest",][2] <- paste(subset(all_sites, substring(all_sites, 2, 2) == "E" & substring(all_sites, 1, 1) == "4"), collapse = ";")
  sites_first[sites_first$Basin == "HaidaGwaii",][2] <- paste(NA, collapse = ";")

  # basin not on the map currenly - future
  sites_first[sites_first$Basin == "Nicola_old",][2] <- paste(c("1C01",	"1C09",	"1C19",	"1C25",	"1C29",	"2F13",	"2F18",	"2F23",	"2F24"), collapse = ";")
  sites_first[sites_first$Basin == "FraserPlateau",][2] <- paste(c("1C08", "1C22", "1C21"), collapse = ";")
  sites_first[sites_first$Basin == "LillBridge",][2] <- paste(c("1C06", "1C39", "1C38P", "1C38", "1C40P", "1C40", "1C12P", "1C14P", "1C14", "1C37", "1C05P", "1C05", "1C18P", "1C28"), collapse = ";")
  sites_first[sites_first$Basin == "Quesnel",][2] <- paste(c("1C33A", "1C13A", "1C17", "1C20P", "1C23", "1C41P"), collapse = ";")
  sites_first[sites_first$Basin == "LowerThompson",][2] <- paste(c("1C32", "1C09A", "1C19", "1C25", "1C29", "1C29P", "1C01"), collapse = ";")
  sites_first[sites_first$Basin == "Fraser",][2] <- paste(subset(all_sites, substring(all_sites, 1, 1) == "1"), collapse = ";")
  sites_first[sites_first$Basin == "Province",][2] <- paste(all_sites, collapse = ";")

  if (id == "All") {
    sites_id <- sites_first
  } else {
    # find the basin within the dataframe and return the basin name
    sites_id <- sites_first[sites_first$Station_ID_used %like% id, ] %>%
      dplyr::select(Basin) %>%
      dplyr::distinct(Basin)
  }

  # Subset by the basin you want
  if (basin == "All"){
    sites_final <- sites_id
  } else {
    sites_final <- sites_id %>%
      dplyr::filter(Basin %in% basin)
  }
  return(sites_final)
}

# ===============
#' Return the name of all snow basins within BC
#' @export
#' @keywords snow basin names
#' @examples \dontrun{}
snow_basins <- function() {
  return(c("UpperFraserWest", "UpperFraserEast", "Nechako", "MiddleFraser", "LowerFraser", "NorthThompson",
                  "SouthThompson", "UpperColumbia", "WestKootenay", "EastKootenay", "Okanagan", "Boundary", "Similkameen", "SouthCoast",
                  "VancouverIsland", "CentralCoast", "Skagit", "Peace", "SkeenaNass", "Stikine", "Liard", "Northwest", "HaidaGwaii",
                  "Nicola_old", "FraserPlateau", "LillBridge", "Quesnel", "LowerThompson", "Fraser", "Province"))
}

# ======================
#' Legal annotation for plots
#' @export
#' @keywords internal
#' @examples \dontrun{}
annotation <- function() {
  paste0("<b>Users should use the information on this website with caution and at their own risk.</b>", "<br>",
                     "Reproduction and analysis of data published by the BC Ministry of Environment, including data collected by affilated partners (more information through <a href= 'https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-science-data/water-data-tools/snow-survey-data'>Snow Survey Data)</a>",
                     "<br>", " has not been produced in affiliation with or with the endorsement of the Ministry of Environment.")
}

# =================
#' Colour palette
#' @export
#' @keywords internal
#' @examples \dontrun{}
colour_p <- function() {

  # Colours from Sam Albers's map - March 2019
  colour_hex <- c("#FFFFFF", "#E50000", "#E69800", "#FFD380", "#FFFF00",
                "#AAFF01", "#00FEC4", "#01C5FF", "#0071FE", "#002573")

  colours_v <- viridis::viridis(1000)

  # normal
  colour_norm <- "#482475FF"
  # curent year
  colour_curr <- "#43BE71FF"
  # mean
  colour_mean <- "#7AD151FF"

  colour_palette <- list(colour_hex = colour_hex, colours_v = colours_v, colour_norm = colour_norm, colour_curr = colour_curr, colour_mean = colour_mean)
}

