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

``` r
# Return statistics for 2F18P for March 1, 2021. Use a normal period from 1991-2020. Force the recalculation of the normals.
library(bcsnowstats)

stats_ASWE_west <- get_snow_stats(station_id = "2C09Q",
                                    survey_period = "03-01",
                                    get_year = "2021",
                                    normal_min = 1991,
                                    normal_max = 2020,
                                    force = FALSE)
#> [1] "Calculating statistics for 2C09Q"
#> [1] "SWE archive was updated up to 2021"
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> Joining, by = c("station_id", "m_d")
```

1.  SWE_normals()

The SWE_normals function calculates normals for a defined time span. See
vignette for a detailed explanation of how normals are calculated
(including data coverage thresholds and data filling procedures).

``` r
# Get statistics for 5 manual snow stations for March 1st, 2021. Use a normal period of 1991-2020.

# Calculate normals from 1991-2020 for an automated station 2C09Q - Morrisey Ridge. Force the re-download of data.
normal_aswe <- SWE_normals(data = "2C09Q", normal_max = 2020, normal_min = 1991, force = TRUE)
#> [1] "SWE archive was updated up to 2021"
#> Saving to bcsnowstats data directory at C:\Users\AJOLLYMO\AppData\Local/R/cache/R/bcsnowstats

# Calculate normals from 1991-2020 for an manual station 4E01. Force the re-download of data.
normal_manual <- SWE_normals(data = "4E01", normal_max = 2020, normal_min = 1991, force = TRUE)
#> [1] "Manual SWE archive was updated up to 2021-06-18"
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%  |                                                                              |=================================================================     |  93%  |                                                                              |======================================================================| 100%
#> Rows: 63 Columns: 13
#> -- Column specification --------------------------------------------------------
#> Delimiter: ","
#> chr  (5): Snow Course Name, Number, Survey Code, Snow Line Code, Survey Period
#> dbl  (7): Elev. metres, Snow Depth cm, Water Equiv. mm, Snow Line Elev. m, %...
#> date (1): Date of Survey
#> 
#> i Use `spec()` to retrieve the full column specification for this data.
#> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Joining, by = "station_id"
#> Joining, by = "station_id"
```

##### Visualizations

This package also contains several functions for visualizing data and
statistics from manual and automated snow monitoring.

1.  plot_interactive_aswe This function plots SWE for the current water
    year for a particular ASWE station. The statistics for the station,
    including data for the past years, is also plotted.

``` r
plot_test <- plot_interactive_aswe(id = "1D08P",
                      save = "No")
```

1.  plot_interactive_manual This function plots data from manual
    surveys. The interactive plot also shows statistics for the most
    recent manual snow survey, as well as the previous years data.

``` r
plot_manual <- plot_interactive_manual(id = "1A04",
                                       save = "No")
```

1.  plot_climate_aswe This function plots climate data from ASWE sites
    for the current water year, including daily precipitation, daily max
    and min air temperatures, and daily change in SWE.

``` r
monthly <- plot_monthly_deltaswe(id ="1A01P", 
                      path_save = paste0(drive_Q, "/Real-time_Data/ASP_daily_interactive/ASWE/MonthlyDeltaSWE_boxplots/"))
```

1.  plot_monthly_deltaswe This function plots the monthly change in SWE
    for automated snow survey stations. The monthly change is calculated
    for past months of data, and shows the change in SWE from the first
    to the last day of the month for the current year; the boxplot shows
    how the current year compares to previous years of data.

``` r
monthly <- plot_monthly_deltaswe(id ="1A01P", 
                      path_save = paste0(drive_Q, "/Real-time_Data/ASP_daily_interactive/ASWE/MonthlyDeltaSWE_boxplots/"))
```

1.  plot_interactive_basin This function returns an interactive plot of
    SWE values averaged for all ASWE stations within a particular basin.
    Data for past years is also presented within the same plot.

Exceptions are any sites that the user wishes to skip within the
calculation of basin-wide SWE (such as those with issues). The user also
has the option to save the plot in the location of their specification.

``` r
plot_basin <- plot_interactive_basin(basin = "UpperFraserEast",
                       exceptions = NA,
                       save = "No")
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
