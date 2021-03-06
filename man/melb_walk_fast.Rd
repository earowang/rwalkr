% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soda.R
\name{melb_walk_fast}
\alias{melb_walk_fast}
\title{API using Socrata to Melbourne pedestrian data (per hour)}
\usage{
melb_walk_fast(year = NULL, sensor = NULL, na.rm = FALSE, app_token = NULL)
}
\arguments{
\item{year}{An integer or a vector of integers. By default, it's the current
year.}

\item{sensor}{Sensor names. By default, it pulls all the sensors. Use \link{pull_sensor}
to see the available sensors.}

\item{na.rm}{Logical. \code{FALSE} is the default suggesting to include \code{NA} in
the dataset. \code{TRUE} removes the \code{NA}s.}

\item{app_token}{Characters giving the application token. A limited number of
requests can be made without an app token (\code{NULL}), but they are subject
to much lower throttling limits than request that do include one. Sign up
for an app token \href{https://data.melbourne.vic.gov.au/profile/app_tokens}{here}.}
}
\value{
A tibble including these variables as follows:
\itemize{
\item \code{Sensor}: Sensor name
\item \code{Date_Time}: Date time when the pedestrian counts are recorded
\item \code{Date}: Date associated with \code{Date_Time}
\item \code{Time}: Time of day
\item \code{Count}: Hourly counts
}
}
\description{
API using Socrata to Melbourne pedestrian data (per hour)
}
\details{
It provides the API using \href{https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/mxb8-wn4w}{Socrata},
where counts are uploaded on a monthly basis. The up-to-date data would be
till the previous month. The data is sourced from
\href{https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp}{Melbourne Open Data Portal}.
Please refer to Melbourne Open Data Portal for more details about the dataset and
its policy.
}
\examples{
\dontrun{
# Retrieve the year 2017
melb_walk_fast(year = 2017)

# Retrieve the year 2017 for Southern Cross Station
melb_walk_fast(year = 2017, sensor = "Southern Cross Station")
}
}
\seealso{
\link{melb_walk}
}
