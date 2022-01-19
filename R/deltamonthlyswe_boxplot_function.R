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

# ==================================
#' Do boxplot on data from the data saved
#' Function gets the monthly change in statistics and creates a boxplot of the data that it then saves.
#' Feb 2020 Ashlee Jollymore
#' @param id station ID that you are calculating box plot statistics for
#' @param path_save Where the plot itself should be saved
#' @param save whether to save the plot or not. Defaults to NO, Options are True or False
#' @export
#' @keywords monthly delta SWE
#' @examples \dontrun{}

plot_monthly_deltaswe <- function(id, path_save, save = "no") {

  # get the statistics for the monthly change in SWE. Function returns both data as well as stats (stats = 1, data = 2)
  monthly_df <- getmonthly_deltaSWE(id)
  monthly_deltaSWE_data <- monthly_df[[2]]

  #Get the monthly change in SWE - YTD
  YTD_monthly_deltaSWE <- getmonthly_deltaSWE_YTD(id)

  if (all(is.na(monthly_deltaSWE_data))) {
    print("No data for station")
  } else if (all(is.na(YTD_monthly_deltaSWE))) {
    print("No data for station")
  } else {

    # Format so that the months start in Oct
    monthly_deltaSWE_data$Month_order <- factor(monthly_deltaSWE_data$Month, levels = unique(monthly_deltaSWE_data$Month))

    YTD_monthly_deltaSWE$Month_order <- factor(YTD_monthly_deltaSWE$Month, levels = unique(YTD_monthly_deltaSWE$Month))

    years_data <- min(monthly_df[[1]]$n_years)

    # plot the boxplot
    p <- plotly::plot_ly(data = monthly_deltaSWE_data,
                 x = ~Month_order,
                 y = ~first_diff,
                 type = "box",
                 name = "Historic Monthly Accumulation") %>%
      # Add the monthly accumulation for the YTD
      plotly::add_trace(data = YTD_monthly_deltaSWE,
                x = ~Month_order,
                y = ~first_diff,
                type = "scatter",
                mode = "markers",
                name = "YTD Monthly Accumulation") %>%
      plotly::layout(title = paste0('Change in Monthly SWE (mm) for ', id),
             xaxis = list(
               title = 'Month'),
             yaxis = list(title = 'Change in Monthly SWE (mm/month)')) %>%
      # Add the number of years within the
      plotly::layout(annotations = list(
        list(x = 0 , y = 1, text = paste0("Years of data: ", years_data), showarrow = F, xref='paper', yref='paper')))

    if (save %in% c("True", "true", "T", "TRUE", TRUE)) {
     htmlwidgets::saveWidget(plotly::as_widget(p), paste0(path_save, "MonthlyDeltaSWE_", id, ".html"),
                             selfcontained = F, # for making the finised product smaller and faster to save
                             libdir = NULL, # for making the finised product smaller and faster to save
                             title = paste0("Monthly Delta SWE ", id))
    }
  }
}
