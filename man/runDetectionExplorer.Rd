% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runDetectionExplorer.R
\name{runDetectionExplorer}
\alias{runDetectionExplorer}
\title{Run Detection Data Explorer App}
\usage{
runDetectionExplorer(data = NULL)
}
\arguments{
\item{data}{file path to a CSV file containing detection data or
a dataframe}
}
\description{
Runs a Shiny app that allows users to interactively
find the proper parameters to load a detection dataset using
\link{loadDetectionData} and explore plots for detection data
}
\examples{
if(interactive()) {
  detFile <- system.file('extdata/detectionExample.csv', package='PAMscapes')
  runDetectionExplorer(detFile)
}

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
