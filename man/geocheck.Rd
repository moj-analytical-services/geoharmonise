% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocheck.R
\name{geocheck}
\alias{geocheck}
\title{Compare a set of area names to an ONS-maintained list}
\usage{
geocheck(names, area_type, ONS_filedate)
}
\arguments{
\item{names}{A vector of names to be checked}

\item{area_type}{Type of area (eg. "Local Authority", "Region")}

\item{ONS_filedate}{A date corresponding to the ONS code list you wish to compare to. Run the ONS_geolist() function to obtain a list of valid dates.
This should be in the format eg. "31-Dec-19".}
}
\value{
A list containing three elements: \cr \cr
\strong{matches:} A data frame of area names which match with the ONS list (includes Exact and Partial matches) \cr
\strong{unmatched_names:} A vector of names taken from the "names" input parameter which could not be matched to an ONS name \cr
\strong{unmatched_ONS:} A vector of names taken from the requested ONS list which could not be matced to a name from the vector specified in the "names" input parameter
}
\description{
This function compares a list of geographical area names to an ONS area code list.
It takes as an input a list of names, the area type (eg. "Local Authority" or "Region"), and the date of the ONS list you wish to compare to.
To obtain a list of valid dates, use the function ONS_geolist()
}
