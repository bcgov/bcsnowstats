<!--
Copyright 2021 Province of British Columbia
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->
<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

# bcsnowstats

This package calculates relevant statistics for snow sites in British
Columbia. It uses the package ‘bcsnowdata()’ to first retrieve SWE data
from automated snow weather (ASWE) and manual snow sites, and calculate
statistics (such as mean daily SWE, SWE normals for a defined times
period, and percentiles for SWE values).

### Features

### Installation

### Usage

bcsnowstats() contains two functions for analyzing and retrieving
statistics for snow water equivalent (SWE) within provincial snow
monitoring sites in British Columbia, Canada.

get_snow_stats() calculates statistics for a particular station (either
manual or automated) for snow water equivalent (SWE). Statistics can
either be calculated for the entire data record, or a particular day,
survey period, or year. Statistics include the calculation of normals
for a particular span of time specified by the user. Snow normals can
also be calculated via the second function contained within the package
- SWE_normals(). This function will calculate snow normals for a
particular station for a particular time span specified by the user.
Please see the Snow Normal vignette for a detailed explanation of how
snow normals are calculated, including data filling for stations without
sufficient data (\<20 years of data) over a defined normal time span.

#### Examples

##### Statistics

This package contains two functions for retrieving statistics for
automated and manual snow sites: 1) get_snow_stats() and 2)
SWE_normals().

1.  get_snow_stats()

The get_snow_stats() function returns statistics for a particular
station for a specific day or across the entire period of record. The
station can be either a manual or automated snow monitoring station.

    #> Loading required package: ggplot2
    #> 
    #> Attaching package: 'plotly'
    #> The following object is masked from 'package:ggplot2':
    #> 
    #>     last_plot
    #> The following object is masked from 'package:stats':
    #> 
    #>     filter
    #> The following object is masked from 'package:graphics':
    #> 
    #>     layout
    #> [1] "Calculating statistics for 2C09Q"
    #> Reading the data using the read_csv function from the readr package.
    #> Warning: One or more parsing issues, see `problems()` for details
    #> Rows: 17365 Columns: 108
    #> -- Column specification --------------------------------------------------------
    #> Delimiter: ","
    #> dbl  (105): 1A01P Yellowhead Lake, 1A02P McBride Upper, 1A03P Barkerville, 1...
    #> lgl    (2): 2D10P Gray Creek Upper, 4A12P Tsaydaychi Lake
    #> dttm   (1): DATE(UTC)
    #> 
    #> i Use `spec()` to retrieve the full column specification for this data.
    #> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    #> Reading the data using the read_csv function from the readr package.
    #> 
    #> Rows: 3190 Columns: 92
    #> -- Column specification --------------------------------------------------------
    #> Delimiter: ","
    #> dbl  (91): 1A01P Yellowhead Lake, 1A02P McBride Upper, 1A03P Barkerville, 1A...
    #> dttm  (1): DATE(UTC)
    #> 
    #> i Use `spec()` to retrieve the full column specification for this data.
    #> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    #> Joining, by = c("date_utc", "id", "value", "parameter")
    #> Saving to bcsnowstats data directory at C:\Users\AJOLLYMO\AppData\Local/R/cache/R/bcsnowstats
    #> 
    #> Joining, by = c("id", "m_d")
    #> # A tibble: 1 x 51
    #>   date_utc            id    value parameter    wr m_d   date_dmy   mean_day
    #>   <dttm>              <chr> <dbl> <chr>     <dbl> <chr> <date>        <dbl>
    #> 1 2021-03-01 16:00:00 2C09Q   358 swe        2021 03-01 2021-03-01      358
    #> # ... with 43 more variables: min <dbl>, swe_mean <dbl>, Q5 <dbl>, Q10 <dbl>,
    #> #   Q25 <dbl>, Q50 <dbl>, Q75 <dbl>, Q90 <dbl>, max <dbl>, maxdate <dbl>,
    #> #   mindate <dbl>, data_range <chr>, numberofyears <int>, date_min_utc <dttm>,
    #> #   date_max_utc <dttm>, normal_minimum <dbl>, normal_swe_mean <dbl>,
    #> #   normal_Q5 <dbl>, normal_Q10 <dbl>, normal_Q25 <dbl>, normal_Q50 <dbl>,
    #> #   normal_Q75 <dbl>, normal_Q90 <dbl>, normal_maximum <dbl>,
    #> #   data_range_normal <chr>, normal_datarange_estimated <int>, ...

1.  SWE_normals()

The SWE_normals function calculates normals for a defined time span. See
vignette for a detailed explanation of how normals are calculated
(including data coverage thresholds and data filling procedures).

    #> Reading the data using the read_csv function from the readr package.
    #> Warning: One or more parsing issues, see `problems()` for details
    #> Rows: 17365 Columns: 108
    #> -- Column specification --------------------------------------------------------
    #> Delimiter: ","
    #> dbl  (105): 1A01P Yellowhead Lake, 1A02P McBride Upper, 1A03P Barkerville, 1...
    #> lgl    (2): 2D10P Gray Creek Upper, 4A12P Tsaydaychi Lake
    #> dttm   (1): DATE(UTC)
    #> 
    #> i Use `spec()` to retrieve the full column specification for this data.
    #> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    #> Reading the data using the read_csv function from the readr package.
    #> 
    #> Rows: 3190 Columns: 92
    #> -- Column specification --------------------------------------------------------
    #> Delimiter: ","
    #> dbl  (91): 1A01P Yellowhead Lake, 1A02P McBride Upper, 1A03P Barkerville, 1A...
    #> dttm  (1): DATE(UTC)
    #> 
    #> i Use `spec()` to retrieve the full column specification for this data.
    #> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    #> Joining, by = c("date_utc", "id", "value", "parameter")
    #> Saving to bcsnowstats data directory at C:\Users\AJOLLYMO\AppData\Local/R/cache/R/bcsnowstats
    #> 
    #> Reading the data using the read_csv function from the readr package.
    #> 
    #> Rows: 58014 Columns: 13
    #> -- Column specification --------------------------------------------------------
    #> Delimiter: ","
    #> chr  (5): Snow Course Name, Number, Survey Code, Snow Line Code, Survey Period
    #> dbl  (7): Elev. metres, Snow Depth cm, Water Equiv. mm, Snow Line Elev. m, %...
    #> date (1): Date of Survey
    #> 
    #> i Use `spec()` to retrieve the full column specification for this data.
    #> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    #> Reading the data using the read_csv function from the readr package.
    #> 
    #> Rows: 180 Columns: 13
    #> -- Column specification --------------------------------------------------------
    #> Delimiter: ","
    #> chr  (5): Snow Course Name, Number, Survey Code, Snow Line Code, Survey Period
    #> dbl  (7): Elev. metres, Snow Depth cm, Water Equiv. mm, Snow Line Elev. m, %...
    #> date (1): Date of Survey
    #> 
    #> i Use `spec()` to retrieve the full column specification for this data.
    #> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    #> Joining, by = c("Snow Course Name", "Number", "Elev. metres", "Date of Survey", "Snow Depth cm", "Water Equiv. mm", "Survey Code", "Snow Line Elev. m", "Snow Line Code", "% of Normal", "Density %", "Survey Period", "Normal mm")
    #> Joining, by = "id"
    #> Joining, by = "id"

##### Visualizations

This package also contains several functions for visualizing data and
statistics from manual and automated snow monitoring. The RFC also
produces a map that includes these interactive snow plots. All
interactive plots were made using the plotly R package.

###### Automated Snow Station Visulizations

1.  plot_interactive_aswe This function plots SWE for the current water
    year for a particular ASWE station. The statistics for the station,
    including data for the past years, is also plotted.

``` r
plot_test <- plot_interactive_aswe(id = "1A01P",
                      save = FALSE)
plot_test$SWEplot
```

1.  plot_climate_aswe This function plots climate data from ASWE sites
    for the current water year, including daily precipitation, daily max
    and min air temperatures, and daily change in SWE.

``` r
climate <- plot_climate_aswe(id ="1A01P",
                                 save = FALSE)
climate
```

1.  plot_monthly_deltaswe This function plots the monthly change in SWE
    for automated snow survey stations. The monthly change is calculated
    for past months of data, and shows the change in SWE from the first
    to the last day of the month for the current year; the boxplot shows
    how the current year compares to previous years of data.

``` r
monthly_delta <- plot_monthly_deltaswe(id ="1A01P", 
                      save = FALSE)
monthly_delta
```

###### Manual Snow Station Visualizations

1.  plot_interactive_manual This function plots data from manual
    surveys. The interactive plot also shows statistics for the most
    recent manual snow survey, as well as the previous years data.

``` rplot_manual
plot_manual <- plot_interactive_manual(id = "1B06",
                                       save = FALSE)
plot_manual
```

###### Snow Basin Visualizations

1.  plot_interactive_basin This function returns an interactive plot of
    SWE values averaged for all ASWE stations within a particular basin.
    Data for past years is also presented within the same plot.

Exceptions are any sites that the user wishes to skip within the
calculation of basin-wide SWE (such as those with issues). The user also
has the option to save the plot in the location of their specification.

``` r
plot_basin <- plot_interactive_basin(basin = "UpperFraserEast",
                       exceptions = NA,
                       save = FALSE)
plot_basin
```

### Project Status

In development; may change or evolve.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/bcsnowstats/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2021 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
