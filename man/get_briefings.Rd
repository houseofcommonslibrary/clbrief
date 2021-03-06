% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{get_briefings}
\alias{get_briefings}
\title{Get a tibble of summary data on each briefing}
\usage{
get_briefings(briefings_json)
}
\arguments{
\item{briefings_json}{A list of briefings as returned by
\code{fetch_briefings_json}}
}
\value{
A tibble of summary data on briefings.
}
\description{
\code{get_briefings} takes a list of briefings returned from the API as JSON
and returns a tibble of summary data those briefings, with one row per
briefing.
}
\details{
Note that data which has a one to many realtionship with a briefing is not
included in the tibble returned by this function. See the other data
processing functions for extracting nested data.
}
