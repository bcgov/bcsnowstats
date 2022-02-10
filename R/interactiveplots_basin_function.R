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

#' Function for plotting SWE across an enture basin basin
#' @param basin basin name
#' @param exceptions sites to leave out
#' @param path_basin path to where basin plots should be saved
#' @param save yes or no. Whether to save or not
#' @export
#' @keywords plot interactive SWE by basin
#' @examples \dontrun{}

plot_interactive_basin <- function(basin, exceptions = NA, path_basin, save) {

  basin_input <- basin
  exceptions_daily <- exceptions

  data_initial <- basin_averaged_swe_interactive(basin_input = basin_input,
                                                 exceptions = exceptions_daily)

  if (dim(data_initial)[2] > 2) { # if there is data for the basin, perform statistics

    # This year's data
    data_current <- data_initial %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::filter(wr == bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::filter(m_d != "29-2") # remove leap years

    Date_seq <- data.frame(seq(as.Date(paste0(bcsnowdata::wtr_yr(Sys.Date())-1, "-10-01")), as.Date(paste0(bcsnowdata::wtr_yr(Sys.Date()), "-09-30")), "day"))
    colnames(Date_seq) = "date_utc"

    data_current_NA <- dplyr::full_join(data_current, Date_seq, by = "date_utc")

    # Historic data
    data_historic <- data_initial %>%
      dplyr::ungroup() %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::filter(wr < bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::mutate(wr = as.factor(wr)) %>%
      dplyr::filter(m_d != "29-2")  %>% # remove leap years
      dplyr::mutate(month = lubridate::month(date_utc)) %>%
      dplyr::mutate(year_art = ifelse(month >=10, bcsnowdata::wtr_yr(Sys.Date()) - 1, bcsnowdata::wtr_yr(Sys.Date()))) %>%
      dplyr::mutate(Date_art = as.Date(paste0(lubridate::day(date_utc), "-",
                                              lubridate::month(date_utc), "-", year_art),
                                       format = "%d-%m-%Y")) %>%
      dplyr::arrange(wr, Date_art) %>%
      dplyr::filter(!is.na(Date_art)) %>%
      dplyr::mutate(Date_art = as.Date(Date_art)) %>%
      #dplyr::mutate(wr_art = wtr_yr(Date_art)) %>%
      dplyr::filter(wtr_yr(Date_art) == bcsnowdata::wtr_yr(Sys.Date())) %>%
      dplyr::filter(!is.na(mean_basin_SWE)) %>%
      dplyr::select(Date_art, mean_basin_SWE, wr, date_utc) %>%
      dplyr::mutate(mean_basin_SWE = as.numeric(mean_basin_SWE)) %>%
      #dplyr::rename(date_utc = Date_art) %>%
      dplyr::filter(!is.na(date_utc)) %>%  # remove leap years
      dplyr::ungroup()

    # Join the historic and the current year back together
    data_current_join <- data_current_NA %>%
      dplyr::mutate(Date_art = date_utc) %>%
      dplyr::mutate(wr = as.factor(wr)) %>%
      dplyr::select(Date_art, mean_basin_SWE, wr, date_utc) %>%
      dplyr::ungroup()

    data_all <- bind_rows(data_historic, data_current_join) %>%
      dplyr::mutate(wr = as.factor(wr), Date_art = as.Date(Date_art), date_utc = as.Date(date_utc),
                    mean_basin_SWE = as.numeric(mean_basin_SWE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(wr)) %>% # attempt to re-arrrange plots so that current years are first
      dplyr::mutate(wr = factor(wr, levels = rev(levels(wr)))) # reverse the order of years to have the most recent at the top

    # Statistics
    # Get only the statistics
    data_stats <- data.frame(data_initial) %>%
      dplyr::mutate(wr = bcsnowdata::wtr_yr(date_utc)) %>%
      dplyr::ungroup() %>%
      dplyr::select("date_utc",  "min", "max","Q10", "Q25", "Q50", "Q75","Q90", "normal_Q50") %>%
      dplyr::mutate(d_m = paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc))) %>%
      dplyr::distinct(d_m, .keep_all = TRUE) %>%
      dplyr::mutate(year_art = ifelse(lubridate::month(date_utc) >=10, bcsnowdata::wtr_yr(Sys.Date()) - 1, bcsnowdata::wtr_yr(Sys.Date()))) %>%
      dplyr::mutate(Date_art = as.Date(paste0(lubridate::day(date_utc), "-", lubridate::month(date_utc), "-", year_art),
                                       format = "%d-%m-%Y")) %>%
      dplyr:: filter(!is.na(date_utc)) %>%
      dplyr::select(-date_utc, -year_art, -d_m) %>%
      dplyr::rename("Normal (1981-2010)" = "normal_Q50") %>%
      dplyr::rename("Median" = "Q50") %>%
      reshape::melt(id = "Date_art") %>%
      dplyr::mutate(d_m = paste0(lubridate::day(Date_art), "-", lubridate::month(Date_art)))

    # calculate the five day rolling mean to smooth
    d_all_stats <- data_stats %>%
      dplyr::arrange(variable, Date_art) %>%
      dplyr::group_by(variable) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(value_5 = zoo::rollapply(data = zoo::na.approx(value), # fill in missing data within the stats
                                        width = 5,
                                        FUN = mean,
                                        align = "center",
                                        fill = NA,
                                        na.rm = T)) %>%
      dplyr:: mutate(Date = format(Date_art, format = "%d-%b")) %>%
      dplyr::rename(date_utc = Date_art)

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

    # Ensure all data rounded
    bands <- round_df(bands, digits = 0) %>%
      dplyr::arrange(date_utc)
    data_historic <- round_df(data_historic, digits = 0) %>%
      dplyr::arrange(wr, date_utc)
    data_current_NA <- round_df(data_current_NA, digits = 0) %>%
      dplyr::arrange(date_utc)
    data_all <- round_df(data_all, digits = 0) %>%
      dplyr::arrange(desc(date_utc))

    # ==================
    # Plot annotations
    # ==================

    ## Calculate the days to peak snow normal
    # day of peak
    if (sum(!is.na(data_initial$normal_Q50)) / length(data_initial$normal_Q50) > 0.5 &&
       length(data_initial$mean_basin_SWE[na.omit(data_initial$date_utc) == Sys.Date()]) > 0) { # if there is a normal for the station and a measuerment for today

      date_max <- na.omit(Q50$date_utc[Q50$Q50 == max(Q50$Q50, na.rm = TRUE)]) # date of max median
      days_till_peak <- as.Date(date_max[1]) - Sys.Date()

      # statistics
      todays_data <- na.omit(data_initial[data_initial$date_utc == Sys.Date(),])
      ## Calculate percent of normal
      percent_normal <- paste0(round(todays_data$mean_basin_SWE / todays_data$normal_Q50*100, digits = 0), "%")

      ## Percent of normal peak
      percent_normal_peak <- round(todays_data$mean_basin_SWE / max(data_initial$normal_Q50, na.rm = TRUE) *100, digits = 0)
      percent_normal_peak <- paste0(percent_normal_peak, "%")

      # Typical percent of normal
      typical_percentnorm <- round(todays_data$normal_Q50 / max(data_initial$normal_Q50, na.rm = TRUE) *100, digits = 0)


    } else {

      date_max <- na.omit(Q50$date_utc[Q50$Q50 == max(Q50$Q50, na.rm = TRUE)]) # date of max median
      days_till_peak <- as.Date(date_max[1]) - Sys.Date()

      todays_data <- na.omit(data_initial[data_initial$date_utc == Sys.Date(),])
      #date_max <- "Insufficient data"
      #days_till_peak <- "Insufficient data"
      #day_peak <- "Insufficient data"
      percent_normal <- "Insufficient data"
      percent_normal_peak <- "Insufficient data"
      typical_percentnorm <- "Insufficient data"
    }
    # Percent of median
    percent_median <- paste0(round(todays_data$mean_basin_SWE / todays_data$Q50 * 100, digits = 0), "%")

    ## Percentile rank
    # Calculate the percentile
    #percentile <- data_initial %>%
    #filter(!is.na(mean_basin_SWE)) %>%
    #   dplyr::mutate(percentile = ifelse(length(unique(year(date_utc))) > 5,
    #                      round(map2_dbl(historic_SWE, mean_basin_SWE, ~ecdf(.x$mean_SWE)(.y))*100, digits = 2), NA))

    if (length(todays_data$mean_basin_SWE) > 0) {
      percentile_today <- round(purrr::map2_dbl(todays_data$historic_swe, todays_data$mean_basin_SWE, ~ecdf(.x$mean_SWE)(.y))*100, digits = 0)
    } else {
      percentile_today = NA
    }

    # ======================
    # get the increase in SWE from today's date to the max. Based on previous monthly change in SWE statistics
    # ======================
    data_all <- data_all %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::arrange(date_utc) %>%
      dplyr::mutate(value = mean_basin_SWE) %>%
      dplyr::mutate(date_dmy = date_utc)

    lastday_data <- data_all$date_utc[max(which(!is.na(data_all$mean_basin_SWE)))]
    date_peak <- date_max[1]

    # If there is a peak date for the basin
    if (class(date_peak) == "Date") {
     deltaSWE <- deltaSWE_datetopeak(data_plot_1 = data_all,
                                    lastday_data = lastday_data,
                                    date_peak = date_peak)
    }

    # ======================
    # Plot
    # ======================
    p <- plotly::plot_ly(colors = colour_p()$colours_v) %>%
      plotly::add_ribbons(data = subset(d_all_stats),
                  x = as.Date(bands$date_utc),
                  ymax = bands$max,
                  ymin = bands$Q90,
                  fillcolor = list(color = colour_p()$colour_hex[9], opacity = 0),
                  opacity = 0.2,
                  line = list(color = colour_p()$colour_hex[9], opacity = 1, width = 2),
                  name = 'Max - Q90') %>%
      plotly::add_ribbons(data = bands,
                  x = as.Date(bands$date_utc),
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
      plotly::add_trace(data = subset(d_all_stats, variable == "MAX"), x = ~date_utc, y = ~as.numeric(value_5),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~variable,
                line = list(color = "rgb(130,130,130)", width = 2, dash = 'dashdot')) %>%
      plotly::add_trace(data = subset(d_all_stats, variable == "MIN"), x = ~date_utc, y = ~as.numeric(value_5),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~variable, line = list(color = "rgb(130,130,130)", width = 2, dash = 'dashdot')) %>%
      plotly::add_trace(data = subset(d_all_stats, variable == "Q25"), x = ~date_utc, y = ~as.numeric(value_5),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~variable,
                line = list(color = "rgb(153,204,255)", width = 2, dash = 'dashdot')) %>%
      plotly::add_trace(data = subset(d_all_stats, variable == "Q75"), x = ~date_utc, y = ~as.numeric(value_5),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~variable,
                line = list(color = "rgb(153,204,255)", width = 2, dash = 'dashdot')) %>%
      plotly::add_trace(data = subset(d_all_stats, variable == "Normal (1981-2010)"), x = ~date_utc, y = ~as.numeric(value_5),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~variable,
                line = list(color = colour_p()$colour_norm, width = 2, dash = 'dash'))  %>% # normal
      plotly::add_trace(data = subset(d_all_stats, variable == "Median"), x = ~date_utc, y = ~as.numeric(value_5),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~variable,
                line = list(color = "rgb(130,130,130)", width = 3, dash = 'dashdot'))  %>% # median (Q50)
      # current year data
      plotly::add_trace(data = subset(data_all, as.numeric(as.character(data_all$wr)) == wtr_yr(Sys.Date())),
                x = ~Date_art, y = ~as.numeric(mean_basin_SWE),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                color = ~wr,
                line = list(color = "black", width = 4),
                inherit = TRUE) %>% # current year
      # past year data
      plotly::add_trace(data = subset(data_all, as.numeric(as.character(data_all$wr)) < wtr_yr(Sys.Date())),
                x = ~as.Date(Date_art),
                y = ~as.numeric(mean_basin_SWE),
                type = 'scatter', mode = 'lines',
                connectgaps = FALSE,
                name = ~wr,
                #color = ~wr,
                showlegend = TRUE, visible = "legendonly",
                inherit = TRUE,
                line = list(color = colour_p()$colours_v, width = 3)) %>%
      # Annotations
      plotly::layout(autosize = T,
             title = paste0('Basin-Averaged SWE (mm) for ', basin_input),
             margin = list(l=30, r=30, b=55, t=30, pad=0),
             xaxis = list(
               title = annotation(),
               titlefont = list(size=8),
               type = 'date',
               range = c(min(data_current_NA$date_utc), max(data_current_NA$date_utc)),
               tickformat = "%d-%B"),
             yaxis = list(title = 'Basin-Averaged SWE (mm)')) %>%
      plotly::layout(annotations = list(
        list(x = 0 , y = 1.0, text = paste0("Current % of normal (1981-2010): ", percent_normal, " | Current % of median: ", percent_median), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.975, text = paste0("% of normal peak: ", percent_normal_peak, " | Typical % of peak accumulation for today: ", typical_percentnorm), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.95, text = paste0("Day of peak: ", date_max[1], " | Days until normal peak: ", days_till_peak), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.925, text = paste0("Percentile Rank: ", percentile_today, "th"), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.90, text = paste0("*Statistics smoothed by 5-day average | Updated: ", Sys.Date()), showarrow = F, xref='paper', yref='paper'),
        list(x = 0 , y = 0.875, text = paste0("Sites: ", unique(data_initial$sites_used)), showarrow = F, xref='paper', yref='paper')
      ))

    # =============================
    # Add in the projected SWE from last data point to the date of the normal peak
    # =============================
    if (exists("deltaSWE")) {
      if (dim(deltaSWE)[1] > 0){
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

    # Save plot
    if (any(save %in% c("True", "true", "T", "TRUE", TRUE))) {
      htmlwidgets::saveWidget(plotly::as_widget(p),
                              paste0(path_basin, basin, ".html"),
                              selfcontained = F, # for making the finised product smaller and faster to save
                              libdir = NULL, # for making the finised product smaller and faster to save
                              title = paste0(basin, " SWE"))
    }
  } else {
    p <- print(paste0("No data for basin"))
  }
  p
}
