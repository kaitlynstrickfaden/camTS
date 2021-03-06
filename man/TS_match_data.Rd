% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TS_match_data.R
\name{TS_match_data}
\alias{TS_match_data}
\title{Matches observations of two datasets by datetime}
\usage{
TS_match_data(dataset, imagedata)
}
\arguments{
\item{dataset}{a data frame object containing a column called "Datetime". Datetime column must contain POSIXct date-time objects.}

\item{imagedata}{a data frame containing image metadata. Must have a column called "Datetime" containing POSIXct date-time objects and a column called "TriggerMode" with camera trigger mode as character objects (see TS_extract_meta.R).}
}
\value{
A new data frame with dataset and imagedata merged together.
}
\description{
Binds all columns from the second dataset to the first dataset by finding the closest Datetime observations.
}
