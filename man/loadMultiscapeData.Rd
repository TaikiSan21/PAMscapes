% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadMultiscapeData.R
\name{loadMultiscapeData}
\alias{loadMultiscapeData}
\title{Load Multiple Folders of Soundscape Data}
\usage{
loadMultiscapeData(
  x,
  timeBin = NULL,
  binFunction = "median",
  binCount = FALSE,
  octave = c("original", "tol", "ol"),
  label = NULL,
  keepQuals = c(1),
  keepEffort = TRUE,
  dropNonHmd = TRUE,
  tz = "UTC",
  extension = c("nc", "csv")
)
}
\arguments{
\item{x}{a vector of folder names to load}

\item{timeBin}{amount of time to bin data by, format can
be "#Unit" e.g. \code{'2hour'} or \code{'1day'}. Unlike
\link{loadSoundscapeData} this argument is now mandatory
to reduce data size}

\item{binFunction}{summary function to apply to data in each time bin,
default is "median"}

\item{binCount}{logical flag to return the number of times in
each time bin as column "binCount"}

\item{octave}{one of "original", "tol", or "ol". If "original" then
nothing happens, otherwise data are converted to Octave-leve ("ol")
or Third-Octave-Level ("tol") measurements using
\link{createOctaveLevel}}

\item{label}{if not \code{NUL}, then must be of equal length to \code{x}}

\item{keepQuals}{quality flag values to keep. Accepts vector of
integers from (1, 2, 3, 4) corresponding to flag labels "Good",
"Not evaluated/Unknown", "Compromised/Questionable", and "Unusable/Bad".
HMD levels for points with data quality flags outside of \code{keepQuals}
will be marked as \code{NA}.}

\item{keepEffort}{if \code{TRUE} or \code{FALSE}, a logical flag whether or
not to keep the effort information with the outputs (number of seconds
per minute). If a numeric value, then any minutes with an effort value
less than \code{keepEffort} will be removed (e.g. \code{50} will remove
minutes with less than 50 seconds of effort)}

\item{dropNonHmd}{logical flag to drop non-standard hybrid millidecade
bands, only applies to HMD type data. Some datasets have frequency
values that are not part of the standard HMD bands (e.g. at exactly
the Nyquist rate), if \code{TRUE} these will be removed.}

\item{tz}{timezone of the data being loaded, will be converted to UTC
after load}

\item{extension}{only required if both netCDF and CSV files exist in
the folders to load, in which case only one type will be loaded.
Must be one of "nc" or "csv"}
}
\value{
a dataframe
}
\description{
Loads soundscape data just like \link{loadSoundscapeData},
but is designed to load multiple soundscape datasets from
multiple folders. This is identical to loading each folder of
data individually with the same bin and label parameters.
}
\details{
This function is equivalent to loading each folder of data
separately with the same time and octave-level aggregation options
applied, and is meant as a convenient wrapper for loading multiple
years or sites of data for comparison person. The expectation is that
this function will be primarily used for large scale comparisons, hence
why \code{timeBin} is a required argument to reduce data resolution.

The only other difference is that if no labels are supplied for the folders,
then one will be generated either from the names of \code{x} if it is a
named vector, or the name of the folder using \link{basename}. This is to
ensure that each separate folder can be identified once read in.
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
