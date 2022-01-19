
#' @name ASWE_normals_1981t2010
#' @title ASWE Normals 1981-2010
#' @description Normals for ASWE sites from 1981-2010. A dataset containing the previously calculated normals for ASWE sites (1981-2010)
#' @format A data frame with 71 rows and 20 variables:
#' \describe{
#'   \item{STATIONID}{station id}
#'   \item{STATIONNAME}{name of station}
#'   \item{JAN_1_SWE}{SWE normal for Jan 1}
#'   \item{JAN_1_CODE}{Code for Jan 1 SWE normal}
#'   \item{FEB_1_SWE}{SWE normal for Feb 1}
#'   \item{FEB_1_CODE}{Code for Feb 1 SWE normal}
#'   \item{MAR_1_SWE}{SWE normal for March 1}
#'   \item{MAR_1_CODE}{Code for March 1 SWE normal}
#'   \item{\code{APR_1_SWE}}{double SWE normal for April 1}
#'   \item{\code{APR_1_CODE}}{character Code for April 1 SWE normal}
#'   \item{MAY_1_SWE}{SWE normal for May 1}
#'   \item{MAY_1_CODE}{Code for MAYil 1 SWE normal}
#'   \item{MAY_15_SWE}{SWE normal for May 15}
#'   \item{MAY_15_CODE}{Code for May 15 SWE normal}
#'   \item{JUN_1_SWE}{SWE normal for June 1}
#'   \item{JUN_1_CODE}{Code for June 1 SWE normal}
#'   \item{JUN_15_SWE}{SWE normal for June 15}
#'   \item{JUN_15_CODE}{Code for June 15 SWE normal}
#'   \item{\code{MAX_PEAK_SWE}}{double MAX peak SWE}
#'   \item{\code{MIN_PEAK_SWE}}{double MIN peak SWE}
#' }
"ASWE_normals_1981t2010"

#' @name manual_normals_1981t2010
#' @title Manual normals 1981-2010
#' @description SWE normals for manual sites from 1981-2010. A dataset containing the previously calculated normals for manual snow sites (1981-2010)
#' @format A data frame with 407 rows and 38 variables:
#' \describe{
#'   \item{STATIONID}{station id}
#'   \item{STATIONNAME}{name of station}
#'   \item{JAN_1_DEPTH}{Snow depth normal for Jan 1}
#'   \item{JAN_1_SWE}{SWE normal for Jan 1}
#'   \item{JAN_1_DENSITY}{Density normal for Jan 1}
#'   \item{JAN_1_CODE}{Code for Jan 1 SWE normal}
#'   \item{FEB_1_DEPTH}{Snow depth normal for FEB 1}
#'   \item{FEB_1_SWE}{SWE normal for FEB 1}
#'   \item{FEB_1_DENSITY}{Density normal for FEB 1}
#'   \item{FEB_1_CODE}{Code for FEB 1 SWE normal}
#'   \item{MAR_1_DEPTH}{Snow depth normal for MAR 1}
#'   \item{MAR_1_SWE}{SWE normal for MAR 1}
#'   \item{MAR_1_DENSITY}{Density normal for MAR 1}
#'   \item{MAR_1_CODE}{Code for MAR 1 SWE normal}
#'   \item{APR_1_DEPTH}{Snow depth normal for APR 1}
#'   \item{APR_1_SWE}{SWE normal for APR 1}
#'   \item{APR_1_DENSITY}{Density normal for APR 1}
#'   \item{APR_1_CODE}{Code for APR 1 SWE normal}
#'   \item{MAY_1_DEPTH}{Snow depth normal for MAY 1}
#'   \item{MAY_1_SWE}{SWE normal for MAY 1}
#'   \item{MAY_1_DENSITY}{Density normal for MAY 1}
#'   \item{MAY_1_CODE}{Code for MAY 1 SWE normal}
#'   \item{MAY_15_DEPTH}{Snow depth normal for MAY 15}
#'   \item{MAY_15_SWE}{SWE normal for MAY 15}
#'   \item{MAY_15_DENSITY}{Density normal for MAY 15}
#'   \item{MAY_15_CODE}{Code for MAY 15 SWE normal}
#'   \item{JUN_1_DEPTH}{Snow depth normal for June 1}
#'   \item{JUN_1_SWE}{SWE normal for June 1}
#'   \item{JUN_1_DENSITY}{Density normal for JUNE 1}
#'   \item{JUN_1_CODE}{Code for JUNE 1 SWE normal}
#'   \item{JUN_15_DEPTH}{Snow depth normal for June 15}
#'   \item{JUN_15_SWE}{SWE normal for June 15}
#'   \item{JUN_15_DENSITY}{Density normal for JUNE 15}
#'   \item{JUN_15_CODE}{Code for June 1 SWE normal}
#'   \item{\code{MAX_PEAK_DEPTH}}{double max peak depth}
#'   \item{\code{MIN_PEAK_DEPTH}}{double min peak depth}
#'   \item{\code{MAX_PEAK_SWE}}{double max peak SWE}
#'   \item{\code{MIN_PEAK_SWE}}{double min peak SWE}
#' }
"manual_normals_1981t2010"

#' @name meta
#' @title Station Metadata
#' @description Meta data for snow stations in BC, containing especially who owns the station
#' @format A data frame with 102 rows and 8 variables:
#' \describe{
#'   \item{ID}{station id}
#'   \item{BASIN}{basin}
#'   \item{ELEVATION}{Elevation}
#'   \item{LATITUDE}{latitude}
#'   \item{LONGITUDE}{longitude}
#'   \item{YEAR}{year}
#'   \item{NESDIS}{nesdis}
#'   \item{OWNER}{owner}
#' }
"meta"
