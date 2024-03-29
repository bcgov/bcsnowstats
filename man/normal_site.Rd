% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normal_site_function.R
\name{normal_site}
\alias{normal_site}
\title{Function for retrieving data for a specific station. Site can be either manual or snow site. Used when you don't have data for a specific station - can be used to directly retrieve snow normal data.}
\usage{
normal_site(site, normal_max, normal_min, force = FALSE)
}
\arguments{
\item{site}{Site ID. Can be either manual or ASWE site}

\item{normal_max}{Max year that the normal period spans}

\item{normal_min}{Min year that the normal period spans}

\item{force}{whether you want to update the cache. Defaults to FALSE, or no}
}
\description{
Function for retrieving data for a specific station. Site can be either manual or snow site. Used when you don't have data for a specific station - can be used to directly retrieve snow normal data.
}
\examples{
\dontrun{}
}
\keyword{internal}
